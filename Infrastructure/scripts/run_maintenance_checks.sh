#!/usr/bin/env bash
#
# Run every maintenance check, continue past failures, print a per-check
# summary, and exit non-zero if any check failed.
#
# This is the engine behind `make maintenance-check`. Standalone Make
# targets (`make brain-check`, `make parity-check`, `make literature-check`)
# remain fail-fast — they're meant for debugging a single check. This
# orchestrator is for the comprehensive sweep, where seeing every failure
# at once is more useful than stopping at the first.
#
# Adding a new check = appending one entry to CHECK_NAMES and CHECK_COMMANDS
# at the top of this file. No other change is required.

set -u
set -o pipefail
# Deliberately NOT `set -e`: we want to continue past failures.

# ---------------------------------------------------------------------------
# Color handling: ANSI when stdout is a TTY, plain text otherwise (so CI
# logs and pipes stay clean).
# ---------------------------------------------------------------------------
if [[ -t 1 ]]; then
  C_RED=$'\033[31m'
  C_GREEN=$'\033[32m'
  C_DIM=$'\033[2m'
  C_BOLD=$'\033[1m'
  C_RESET=$'\033[0m'
else
  C_RED=""
  C_GREEN=""
  C_DIM=""
  C_BOLD=""
  C_RESET=""
fi

# ---------------------------------------------------------------------------
# Locate repo root and run from there so check commands resolve their
# Infrastructure/... paths consistently regardless of invocation cwd.
# ---------------------------------------------------------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_ROOT"

# ---------------------------------------------------------------------------
# Check registry. The 7 underlying scripts are listed explicitly here
# (rather than calling `make parity-check` etc.) so that:
#   1. A partial parity-check failure shows exactly which sub-script failed.
#   2. The orchestrator owns the order and continue-on-failure semantics.
#   3. Sub-make process overhead and "Entering directory..." noise are avoided.
# ---------------------------------------------------------------------------
CHECK_NAMES=(
  "brain-check"
  "skill-parity"
  "metadata-coverage"
  "canonical-consistency"
  "literature-catalog"
  "internal-paths"
  "catalog-staleness"
)

CHECK_COMMANDS=(
  "bash Infrastructure/scripts/manage_generated_copies.sh check"
  "python3 Infrastructure/scripts/check_skill_parity.py"
  "python3 Infrastructure/scripts/check_adapter_metadata_coverage.py"
  "python3 Infrastructure/scripts/check_canonical_consistency.py"
  "python3 Infrastructure/scripts/validate_literature_catalog.py"
  "python3 Infrastructure/scripts/check_internal_path_references.py"
  "python3 Infrastructure/scripts/check_catalog_staleness.py"
)

# ---------------------------------------------------------------------------
# Signal handling: abort the whole suite on Ctrl-C rather than recording
# the killed check as a normal failure and continuing into the next one.
# ---------------------------------------------------------------------------
trap 'printf "\n%sInterrupted; aborting maintenance suite.%s\n" "$C_RED" "$C_RESET" >&2; exit 130' INT TERM

# ---------------------------------------------------------------------------
# Helpers. Use python3 for sub-second timing (BSD `date` on macOS lacks
# %N support). python3 is already a hard dependency of the suite.
# ---------------------------------------------------------------------------
now() {
  python3 -c "import time; print(time.time())"
}

fmt_duration() {
  python3 -c "
import sys
d = float(sys.argv[1])
print(f'{int(d*1000)}ms' if d < 1 else f'{d:.1f}s')
" "$1"
}

divider() {
  printf '%*s\n' 70 '' | tr ' ' '='
}

# ---------------------------------------------------------------------------
# Run each check, capture status and timing, stream output inline.
# ---------------------------------------------------------------------------
TOTAL=${#CHECK_NAMES[@]}
STATUSES=()
DURATIONS=()
EXIT_CODES=()

OVERALL_START="$(now)"

for i in "${!CHECK_NAMES[@]}"; do
  name="${CHECK_NAMES[$i]}"
  cmd="${CHECK_COMMANDS[$i]}"
  step=$((i + 1))

  printf '\n%s==> [%d/%d] %s%s\n' "$C_BOLD" "$step" "$TOTAL" "$name" "$C_RESET"
  printf '%s    %s%s\n' "$C_DIM" "$cmd" "$C_RESET"

  check_start="$(now)"
  # `bash -c` runs the command string in a subshell; its exit code is captured
  # without propagating to this orchestrator (no `set -e`).
  bash -c "$cmd"
  check_exit=$?
  check_end="$(now)"

  duration="$(python3 -c "print($check_end - $check_start)")"
  duration_fmt="$(fmt_duration "$duration")"
  DURATIONS+=("$duration")
  EXIT_CODES+=("$check_exit")

  if [[ "$check_exit" -eq 0 ]]; then
    STATUSES+=(0)
    printf '%s    PASS%s  (%s)\n' "$C_GREEN" "$C_RESET" "$duration_fmt"
  else
    STATUSES+=(1)
    printf '%s    FAIL%s  (%s, exit=%d)\n' "$C_RED" "$C_RESET" "$duration_fmt" "$check_exit"
  fi
done

OVERALL_END="$(now)"
TOTAL_DURATION_FMT="$(fmt_duration "$(python3 -c "print($OVERALL_END - $OVERALL_START)")")"

# ---------------------------------------------------------------------------
# Summary: aggregate counts, then a status table with failures listed first
# so they're visible at the bottom of the terminal without scrolling back.
# ---------------------------------------------------------------------------
passed=0
failed=0
for s in "${STATUSES[@]}"; do
  if [[ "$s" -eq 0 ]]; then
    passed=$((passed + 1))
  else
    failed=$((failed + 1))
  fi
done

printf '\n%s' "$C_BOLD"
divider
printf '%s' "$C_RESET"

if [[ "$failed" -gt 0 ]]; then
  printf '%sSummary%s  %d passed, %s%d failed%s in %s total\n' \
    "$C_BOLD" "$C_RESET" "$passed" "$C_RED" "$failed" "$C_RESET" "$TOTAL_DURATION_FMT"
else
  printf '%sSummary%s  %s%d passed%s, %d failed in %s total\n' \
    "$C_BOLD" "$C_RESET" "$C_GREEN" "$passed" "$C_RESET" "$failed" "$TOTAL_DURATION_FMT"
fi

printf '%s' "$C_BOLD"
divider
printf '%s' "$C_RESET"

# Failures first.
for i in "${!CHECK_NAMES[@]}"; do
  if [[ "${STATUSES[$i]}" -eq 1 ]]; then
    printf '  %sFAIL%s  %-26s  (%s, exit=%d)\n' \
      "$C_RED" "$C_RESET" "${CHECK_NAMES[$i]}" \
      "$(fmt_duration "${DURATIONS[$i]}")" "${EXIT_CODES[$i]}"
  fi
done

# Then passes.
for i in "${!CHECK_NAMES[@]}"; do
  if [[ "${STATUSES[$i]}" -eq 0 ]]; then
    printf '  %sPASS%s  %-26s  (%s)\n' \
      "$C_GREEN" "$C_RESET" "${CHECK_NAMES[$i]}" \
      "$(fmt_duration "${DURATIONS[$i]}")"
  fi
done

# ---------------------------------------------------------------------------
# Aggregate exit code. Contract preserved: exit 0 iff every check passed.
# ---------------------------------------------------------------------------
if [[ "$failed" -gt 0 ]]; then
  exit 1
fi
exit 0
