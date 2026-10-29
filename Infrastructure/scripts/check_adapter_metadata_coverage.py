#!/usr/bin/env python3
"""Verify canonical adapter metadata TSVs cover every canonical surface.

The Codex native subagent wrappers (`.codex/agents/<name>.toml`) are
generated from `Infrastructure/adapter_configs/codex_agent_metadata.tsv`.
The Claude subagent frontmatter (name, description, tools) is generated
from `Infrastructure/adapter_configs/claude_agent_frontmatter.tsv`.
The Claude slash-command frontmatter (description, argument-hint,
allowed-tools) is generated from
`Infrastructure/adapter_configs/claude_command_frontmatter.tsv`.
The Claude SKILL frontmatter (argument-hint, allowed-tools) for each skill
is generated from `Infrastructure/adapter_configs/claude_skill_frontmatter.tsv`.
All four TSVs apply silent defaults when an entry is missing, which means
a contributor can add a canonical agent, command, or skill, run
`make brain-sync`, and get an under-configured wrapper without any error.

This script enforces explicit coverage:

1. Every canonical agent under Infrastructure/agents/ must have a row in
   codex_agent_metadata.tsv AND in claude_agent_frontmatter.tsv.
2. Every canonical command under Infrastructure/commands/ must have a row
   in claude_command_frontmatter.tsv.
3. Every canonical skill under Infrastructure/skills/ and
   Infrastructure/style/skills/ must have a row in
   claude_skill_frontmatter.tsv.
4. No TSV row may name an entity that no longer exists at the canonical
   layer (orphan rows).

It additionally enforces row-level quality:

5. Every TSV row must have the right number of tab-separated fields and
   every required field must be non-empty. A row that silently relies on
   the script-level defaults goes against the explicit-coverage principle.
6. The description field of every TSV row must not be byte-identical to
   the first non-heading line of the entity's canonical body. The
   description is shown in the picker / autocomplete and should be a
   polished, action-oriented summary distinct from the body opening,
   which is written for the agent reading the file. A lazy copy-paste
   defeats the purpose of having a separate description column.

Use this as part of `make maintenance-check`. It is the explicit gate that
forces contributors to consciously assign reasoning effort to a new agent
and to consciously declare a skill's argument hint.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Callable, Dict, List, Optional, Set, Tuple


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Check adapter metadata coverage.")
    parser.add_argument(
        "--repo-root",
        default=None,
        help="Repository root path. Defaults to script-derived root.",
    )
    return parser.parse_args()


def discover_canonical_basenames(dir_path: Path) -> Set[str]:
    if not dir_path.is_dir():
        return set()
    out: Set[str] = set()
    for entry in dir_path.iterdir():
        if entry.is_file() and entry.suffix == ".md" and entry.name != "README.md":
            out.add(entry.stem)
    return out


def discover_canonical_skill_names(dir_path: Path) -> Set[str]:
    if not dir_path.is_dir():
        return set()
    out: Set[str] = set()
    for entry in dir_path.iterdir():
        if entry.is_file() and entry.suffix == ".md" and entry.name != "README.md":
            out.add(entry.stem)
        elif entry.is_dir() and (entry / "SKILL.md").is_file():
            out.add(entry.name)
    return out


def read_tsv_first_columns(tsv_path: Path) -> Set[str]:
    """Return the set of values in the first column of a TSV, ignoring comments."""
    if not tsv_path.is_file():
        raise FileNotFoundError(f"Missing canonical metadata file: {tsv_path}")
    out: Set[str] = set()
    for raw_line in tsv_path.read_text(encoding="utf-8").splitlines():
        line = raw_line.rstrip("\n")
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        first = line.split("\t", 1)[0].strip()
        if first:
            out.add(first)
    return out


def read_tsv_rows(tsv_path: Path) -> List[Tuple[int, List[str]]]:
    """Return non-comment TSV rows as (1-based line number, list of fields).

    Trailing newline characters are stripped but inter-field whitespace is
    preserved. Comments (lines starting with `#`) and blank lines are skipped.
    """
    if not tsv_path.is_file():
        raise FileNotFoundError(f"Missing canonical metadata file: {tsv_path}")
    rows: List[Tuple[int, List[str]]] = []
    for line_no, raw_line in enumerate(tsv_path.read_text(encoding="utf-8").splitlines(), start=1):
        if not raw_line.strip() or raw_line.lstrip().startswith("#"):
            continue
        rows.append((line_no, raw_line.split("\t")))
    return rows


def first_non_heading_line(path: Path) -> str:
    """Return the first non-empty, non-heading Markdown line from `path`.

    Skips YAML frontmatter (`---` ... `---`), heading lines (`#`-prefixed),
    horizontal rules (`---`), and blank lines. Returns the stripped line.
    Returns the empty string if the file is missing or contains no eligible
    line.
    """
    if not path.is_file():
        return ""
    text = path.read_text(encoding="utf-8")
    in_frontmatter = False
    for i, raw in enumerate(text.splitlines()):
        stripped = raw.strip()
        if i == 0 and stripped == "---":
            in_frontmatter = True
            continue
        if in_frontmatter:
            if stripped == "---":
                in_frontmatter = False
            continue
        if not stripped:
            continue
        if stripped.startswith("#"):
            continue
        if stripped == "---":
            continue
        return stripped
    return ""


# TSV schema registry. Each entry declares the expected column layout, the
# subset of fields that must be non-empty, the 0-based index of the
# description column (the one that gets surfaced in the picker), and a
# resolver from row name to canonical body file (for lazy-copy detection).
#
# The resolver lambdas receive (infra_root_Path, row_name_str) and return
# the Path to compare the description against, or None if no canonical body
# applies for that TSV (skip the lazy-copy check).
TSV_SCHEMAS: Dict[str, Dict[str, object]] = {
    "codex_agent_metadata.tsv": {
        "columns": ["name", "reasoning_effort", "sandbox_mode", "description"],
        "required_non_empty": {"name", "reasoning_effort", "sandbox_mode", "description"},
        "description_col": 3,
        "resolve_body": lambda infra, name: infra / "agents" / f"{name}.md",
    },
    "claude_agent_frontmatter.tsv": {
        "columns": ["name", "tools_csv", "description"],
        "required_non_empty": {"name", "tools_csv", "description"},
        "description_col": 2,
        "resolve_body": lambda infra, name: infra / "agents" / f"{name}.md",
    },
    "claude_command_frontmatter.tsv": {
        "columns": ["name", "argument_hint", "tools_csv", "description"],
        # argument_hint may be intentionally blank for commands that take no args.
        "required_non_empty": {"name", "tools_csv", "description"},
        "description_col": 3,
        "resolve_body": lambda infra, name: infra / "commands" / f"{name}.md",
    },
    "claude_skill_frontmatter.tsv": {
        "columns": ["name", "argument_hint", "allowed_tools_csv", "description"],
        "required_non_empty": {"name", "argument_hint", "allowed_tools_csv", "description"},
        "description_col": 3,
        "resolve_body": lambda infra, name: _resolve_skill_body(infra, name),
    },
}


def _resolve_skill_body(infra: Path, name: str) -> Optional[Path]:
    """Return the canonical body path for a skill (flat or structured, under
    either Infrastructure/skills/ or Infrastructure/style/skills/)."""
    for base in (infra / "skills", infra / "style" / "skills"):
        flat = base / f"{name}.md"
        if flat.is_file():
            return flat
        structured = base / name / "SKILL.md"
        if structured.is_file():
            return structured
    return None


def validate_tsv_row_quality(
    tsv_path: Path,
    schema: Dict[str, object],
    infra_root: Path,
) -> List[str]:
    """Apply checks A (schema) and B (lazy-copy) to every row of `tsv_path`.

    Returns a list of human-readable failure messages. Empty list = clean.
    """
    failures: List[str] = []
    columns: List[str] = schema["columns"]  # type: ignore[assignment]
    required: Set[str] = schema["required_non_empty"]  # type: ignore[assignment]
    description_col: int = schema["description_col"]  # type: ignore[assignment]
    resolve_body: Callable[[Path, str], Optional[Path]] = schema["resolve_body"]  # type: ignore[assignment]
    expected_field_count = len(columns)
    tsv_rel = tsv_path.name

    try:
        rows = read_tsv_rows(tsv_path)
    except FileNotFoundError as exc:
        return [str(exc)]

    for line_no, fields in rows:
        row_label = f"{tsv_rel}:{line_no}"
        if len(fields) != expected_field_count:
            failures.append(
                f"{row_label}: malformed row — expected {expected_field_count} "
                f"tab-separated fields ({', '.join(columns)}), got {len(fields)}. "
                f"Check for missing or extra tabs."
            )
            # Skip downstream checks on a malformed row; field indices are unreliable.
            continue

        # (A) Required field non-emptiness.
        for idx, col_name in enumerate(columns):
            if col_name in required and not fields[idx].strip():
                failures.append(
                    f"{row_label}: required field `{col_name}` is empty for "
                    f"entry '{fields[0]}'. Defaults are reserved for genuinely "
                    f"missing rows, not blank cells."
                )

        # (B) Description must not be byte-identical to the canonical body's
        # first non-heading line.
        name = fields[0].strip()
        description = fields[description_col].strip()
        if not name or not description:
            continue  # Already flagged above as a required-field failure.
        body_path = resolve_body(infra_root, name)
        if body_path is None:
            continue  # No canonical body to compare (e.g., orphan; already flagged).
        body_first_line = first_non_heading_line(body_path)
        if body_first_line and description == body_first_line:
            try:
                body_display = body_path.relative_to(infra_root.parent)
            except ValueError:
                body_display = body_path
            failures.append(
                f"{row_label}: description is byte-identical to the first "
                f"non-heading line of `{body_display}`. The description is "
                f"shown in the picker and should be a polished, distinct "
                f"summary — not a copy of the body opening (which is written "
                f"for the agent reading the file). Rewrite the TSV description "
                f"as a trigger-oriented summary."
            )

    return failures


def main() -> int:
    args = parse_args()
    script_root = Path(__file__).resolve().parents[2]
    repo_root = Path(args.repo_root).resolve() if args.repo_root else script_root

    if not repo_root.exists():
        print(f"ERROR: repo root does not exist: {repo_root}", file=sys.stderr)
        return 1

    infra = repo_root / "Infrastructure"
    adapter_configs = infra / "adapter_configs"

    failures: List[str] = []
    summary: List[Tuple[str, int]] = []

    # Codex agent metadata coverage.
    canonical_agents = discover_canonical_basenames(infra / "agents")
    summary.append(("Canonical agents", len(canonical_agents)))
    try:
        agent_tsv = read_tsv_first_columns(adapter_configs / "codex_agent_metadata.tsv")
    except FileNotFoundError as exc:
        failures.append(str(exc))
        agent_tsv = set()

    summary.append(("Codex agent TSV rows", len(agent_tsv)))
    for missing in sorted(canonical_agents - agent_tsv):
        failures.append(
            f"Canonical agent '{missing}' has no row in "
            f"Infrastructure/adapter_configs/codex_agent_metadata.tsv. "
            f"Add a row specifying reasoning_effort, sandbox_mode, and description."
        )
    for orphan in sorted(agent_tsv - canonical_agents):
        failures.append(
            f"Orphan TSV row in codex_agent_metadata.tsv: '{orphan}' has no "
            f"matching Infrastructure/agents/<name>.md. Remove the row or "
            f"restore the canonical agent file."
        )

    # Claude agent frontmatter coverage.
    try:
        claude_agent_tsv = read_tsv_first_columns(adapter_configs / "claude_agent_frontmatter.tsv")
    except FileNotFoundError as exc:
        failures.append(str(exc))
        claude_agent_tsv = set()

    summary.append(("Claude agent TSV rows", len(claude_agent_tsv)))
    for missing in sorted(canonical_agents - claude_agent_tsv):
        failures.append(
            f"Canonical agent '{missing}' has no row in "
            f"Infrastructure/adapter_configs/claude_agent_frontmatter.tsv. "
            f"Add a row specifying tools and description so the agent registers "
            f"as a valid Claude subagent_type."
        )
    for orphan in sorted(claude_agent_tsv - canonical_agents):
        failures.append(
            f"Orphan TSV row in claude_agent_frontmatter.tsv: '{orphan}' has no "
            f"matching Infrastructure/agents/<name>.md. Remove the row or "
            f"restore the canonical agent file."
        )

    # Claude command frontmatter coverage.
    canonical_commands = discover_canonical_basenames(infra / "commands")
    summary.append(("Canonical commands", len(canonical_commands)))
    try:
        claude_command_tsv = read_tsv_first_columns(adapter_configs / "claude_command_frontmatter.tsv")
    except FileNotFoundError as exc:
        failures.append(str(exc))
        claude_command_tsv = set()

    summary.append(("Claude command TSV rows", len(claude_command_tsv)))
    for missing in sorted(canonical_commands - claude_command_tsv):
        failures.append(
            f"Canonical command '{missing}' has no row in "
            f"Infrastructure/adapter_configs/claude_command_frontmatter.tsv. "
            f"Add a row specifying argument_hint, tools, and description so the "
            f"slash command shows metadata in the Claude Code picker."
        )
    for orphan in sorted(claude_command_tsv - canonical_commands):
        failures.append(
            f"Orphan TSV row in claude_command_frontmatter.tsv: '{orphan}' has "
            f"no matching Infrastructure/commands/<name>.md. Remove the row or "
            f"restore the canonical command file."
        )

    # Claude skill frontmatter coverage.
    canonical_skills = discover_canonical_skill_names(infra / "skills") | discover_canonical_skill_names(infra / "style" / "skills")
    summary.append(("Canonical skills", len(canonical_skills)))
    try:
        skill_tsv = read_tsv_first_columns(adapter_configs / "claude_skill_frontmatter.tsv")
    except FileNotFoundError as exc:
        failures.append(str(exc))
        skill_tsv = set()

    summary.append(("Claude skill TSV rows", len(skill_tsv)))
    for missing in sorted(canonical_skills - skill_tsv):
        failures.append(
            f"Canonical skill '{missing}' has no row in "
            f"Infrastructure/adapter_configs/claude_skill_frontmatter.tsv. "
            f"Add a row specifying argument_hint, allowed_tools, and description."
        )
    for orphan in sorted(skill_tsv - canonical_skills):
        failures.append(
            f"Orphan TSV row in claude_skill_frontmatter.tsv: '{orphan}' has no "
            f"matching Infrastructure/skills/<name>(.md|/SKILL.md) or "
            f"Infrastructure/style/skills/<name>(.md|/SKILL.md). Remove the row "
            f"or restore the canonical skill."
        )

    # Row-level quality: schema conformance + lazy-copy detection.
    # Each TSV is validated against its declared schema in TSV_SCHEMAS.
    rows_validated = 0
    for tsv_filename, schema in TSV_SCHEMAS.items():
        tsv_path = adapter_configs / tsv_filename
        try:
            rows = read_tsv_rows(tsv_path)
            rows_validated += len(rows)
        except FileNotFoundError as exc:
            failures.append(str(exc))
            continue
        failures.extend(validate_tsv_row_quality(tsv_path, schema, infra))
    summary.append(("TSV rows row-level checked", rows_validated))

    if failures:
        print(
            f"Adapter metadata coverage check failed: {len(failures)} issue(s).",
            file=sys.stderr,
        )
        for f in failures:
            print(f"- {f}", file=sys.stderr)
        return 1

    summary_str = ", ".join(f"{label}: {count}" for label, count in summary)
    print(f"Adapter metadata coverage check passed ({summary_str}).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
