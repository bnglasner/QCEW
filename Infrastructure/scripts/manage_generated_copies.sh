#!/usr/bin/env bash

set -euo pipefail

MODE="${1:-sync}"

if [[ "$MODE" != "sync" && "$MODE" != "check" ]]; then
  echo "Usage: $0 [sync|check]" >&2
  exit 2
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
INFRA_ROOT="$REPO_ROOT/Infrastructure"
ADAPTER_CONFIGS="$INFRA_ROOT/adapter_configs"
ROOT_INSTRUCTIONS="$INFRA_ROOT/root_instructions"
CODEX_AGENT_TSV="$ADAPTER_CONFIGS/codex_agent_metadata.tsv"
CLAUDE_SKILL_TSV="$ADAPTER_CONFIGS/claude_skill_frontmatter.tsv"
CLAUDE_AGENT_TSV="$ADAPTER_CONFIGS/claude_agent_frontmatter.tsv"
CLAUDE_COMMAND_TSV="$ADAPTER_CONFIGS/claude_command_frontmatter.tsv"

DEFAULT_CODEX_AGENT_REASONING="medium"
DEFAULT_CODEX_AGENT_SANDBOX="workspace-write"
DEFAULT_CLAUDE_SKILL_ARGUMENT_HINT="[task scope]"
DEFAULT_CLAUDE_SKILL_ALLOWED_TOOLS="Read,Write,Edit,Bash,Glob"
DEFAULT_CLAUDE_AGENT_TOOLS="Read,Write,Edit,Bash,Glob,Grep"
DEFAULT_CLAUDE_COMMAND_ARGUMENT_HINT=""
DEFAULT_CLAUDE_COMMAND_TOOLS="Read,Write,Edit,Bash,Glob,Grep"

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

###############################################################################
# Discovery helpers
###############################################################################

discover_markdown_basenames() {
  local dir="$1"

  if [[ ! -d "$dir" ]]; then
    echo "Missing directory: $dir" >&2
    exit 1
  fi

  find "$dir" -maxdepth 1 -type f -name '*.md' ! -name 'README.md' -exec basename {} \; | LC_ALL=C sort
}

# Discover canonical skills, supporting both flat (foo.md) and structured
# (foo/SKILL.md) layouts. Emits one skill name per line.
discover_skill_names() {
  local dir="$1"

  if [[ ! -d "$dir" ]]; then
    echo "Missing directory: $dir" >&2
    exit 1
  fi

  {
    find "$dir" -maxdepth 1 -type f -name '*.md' ! -name 'README.md' -exec basename {} \; \
      | sed 's/\.md$//'
    # Structured: dir/<skill_name>/SKILL.md
    while IFS= read -r skill_md; do
      basename "$(dirname "$skill_md")"
    done < <(find "$dir" -mindepth 2 -maxdepth 2 -type f -name 'SKILL.md')
  } | LC_ALL=C sort -u
}

# Returns 0 if the skill is structured (has its own subdirectory under $dir).
skill_is_structured() {
  local dir="$1"
  local name="$2"

  [[ -d "$dir/$name" && -f "$dir/$name/SKILL.md" ]]
}

# Echoes the path to the canonical SKILL.md body for skill $name in $dir,
# preferring the structured form. Returns nonzero if neither exists.
canonical_skill_path() {
  local dir="$1"
  local name="$2"

  if skill_is_structured "$dir" "$name"; then
    echo "$dir/$name/SKILL.md"
    return 0
  fi
  if [[ -f "$dir/$name.md" ]]; then
    echo "$dir/$name.md"
    return 0
  fi
  return 1
}

files_for_part() {
  local part="$1"

  case "$part" in
    agents|commands|rules|templates) discover_markdown_basenames "$INFRA_ROOT/$part" ;;
    *) return 1 ;;
  esac
}

command_has_dedicated_skill() {
  local base="$1"
  local name="${base%.md}"

  [[ -f "$INFRA_ROOT/skills/$base" \
     || -f "$INFRA_ROOT/style/skills/$base" \
     || -d "$INFRA_ROOT/skills/$name" \
     || -d "$INFRA_ROOT/style/skills/$name" ]]
}

assert_unique_generated_skill_names() {
  local skill_names_file="$TMP_DIR/generated_skill_names.txt"
  local duplicates
  local name
  local base

  : > "$skill_names_file"

  while IFS= read -r name; do
    printf '%s\n' "$name" >> "$skill_names_file"
  done < <(discover_skill_names "$INFRA_ROOT/skills")

  while IFS= read -r name; do
    printf '%s\n' "$name" >> "$skill_names_file"
  done < <(discover_skill_names "$INFRA_ROOT/style/skills")

  while IFS= read -r base; do
    if command_has_dedicated_skill "$base"; then
      continue
    fi
    printf '%s\n' "${base%.md}" >> "$skill_names_file"
  done < <(discover_markdown_basenames "$INFRA_ROOT/commands")

  duplicates="$(LC_ALL=C sort "$skill_names_file" | uniq -d)"
  if [[ -n "$duplicates" ]]; then
    echo "Duplicate generated skill names detected:" >&2
    printf '%s\n' "$duplicates" >&2
    exit 1
  fi
}

extract_first_non_heading_line() {
  local src_file="$1"

  awk '
    NF && $0 !~ /^#/ {
      print
      exit
    }
  ' "$src_file"
}

###############################################################################
# Canonical metadata lookup helpers (TSV-backed)
###############################################################################

# Looks up a single field for $name in the codex agent metadata TSV.
# Field names: reasoning_effort, sandbox_mode, description.
# Falls back to the script-level defaults when the row is absent.
lookup_codex_agent_field() {
  local name="$1"
  local field="$2"
  local tsv="$CODEX_AGENT_TSV"
  local default_value=""

  case "$field" in
    reasoning_effort) default_value="$DEFAULT_CODEX_AGENT_REASONING" ;;
    sandbox_mode)     default_value="$DEFAULT_CODEX_AGENT_SANDBOX" ;;
    description)      default_value="Repository specialist for ${name} tasks defined in canonical Infrastructure guidance." ;;
    *)
      echo "Unknown codex agent field: $field" >&2
      exit 1
      ;;
  esac

  if [[ ! -f "$tsv" ]]; then
    echo "Missing canonical metadata file: $tsv" >&2
    exit 1
  fi

  local value
  value="$(
    awk -F'\t' -v n="$name" -v f="$field" '
      /^#/ { next }
      NF == 0 { next }
      $1 == n {
        if (f == "reasoning_effort") { print $2; exit }
        if (f == "sandbox_mode")     { print $3; exit }
        if (f == "description")      { print $4; exit }
      }
    ' "$tsv"
  )"

  if [[ -z "$value" ]]; then
    value="$default_value"
  fi
  printf '%s\n' "$value"
}

# Looks up a single field for $name in the claude skill frontmatter TSV.
# Field names: argument_hint, allowed_tools, description.
# Falls back to the script-level defaults when the row is absent.
lookup_claude_skill_field() {
  local name="$1"
  local field="$2"
  local tsv="$CLAUDE_SKILL_TSV"
  local default_value=""

  case "$field" in
    argument_hint) default_value="$DEFAULT_CLAUDE_SKILL_ARGUMENT_HINT" ;;
    allowed_tools) default_value="$DEFAULT_CLAUDE_SKILL_ALLOWED_TOOLS" ;;
    description)   default_value="Run the ${name} skill from canonical Infrastructure guidance." ;;
    *)
      echo "Unknown claude skill field: $field" >&2
      exit 1
      ;;
  esac

  if [[ ! -f "$tsv" ]]; then
    echo "Missing canonical metadata file: $tsv" >&2
    exit 1
  fi

  local value
  value="$(
    awk -F'\t' -v n="$name" -v f="$field" '
      /^#/ { next }
      NF == 0 { next }
      $1 == n {
        if (f == "argument_hint") { print $2; exit }
        if (f == "allowed_tools") { print $3; exit }
        if (f == "description")   { print $4; exit }
      }
    ' "$tsv"
  )"

  if [[ -z "$value" ]]; then
    value="$default_value"
  fi
  printf '%s\n' "$value"
}

# Looks up a single field for $name in the claude agent frontmatter TSV.
# Field names: tools, description.
# Falls back to the script-level defaults when the row is absent.
lookup_claude_agent_field() {
  local name="$1"
  local field="$2"
  local tsv="$CLAUDE_AGENT_TSV"
  local default_value=""

  case "$field" in
    tools)       default_value="$DEFAULT_CLAUDE_AGENT_TOOLS" ;;
    description) default_value="Repository specialist defined in Infrastructure/agents/${name}.md." ;;
    *)
      echo "Unknown claude agent field: $field" >&2
      exit 1
      ;;
  esac

  if [[ ! -f "$tsv" ]]; then
    echo "Missing canonical metadata file: $tsv" >&2
    exit 1
  fi

  local value
  value="$(
    awk -F'\t' -v n="$name" -v f="$field" '
      /^#/ { next }
      NF == 0 { next }
      $1 == n {
        if (f == "tools")       { print $2; exit }
        if (f == "description") { print $3; exit }
      }
    ' "$tsv"
  )"

  if [[ -z "$value" ]]; then
    value="$default_value"
  fi
  printf '%s\n' "$value"
}

# Looks up a single field for $name in the claude command frontmatter TSV.
# Field names: argument_hint, tools, description.
# Falls back to the script-level defaults when the row is absent.
# argument_hint may be intentionally empty for commands that take no args;
# the renderer omits the argument-hint line in that case.
lookup_claude_command_field() {
  local name="$1"
  local field="$2"
  local tsv="$CLAUDE_COMMAND_TSV"
  local default_value=""

  case "$field" in
    argument_hint) default_value="$DEFAULT_CLAUDE_COMMAND_ARGUMENT_HINT" ;;
    tools)         default_value="$DEFAULT_CLAUDE_COMMAND_TOOLS" ;;
    description)   default_value="Run the ${name} playbook from canonical Infrastructure guidance." ;;
    *)
      echo "Unknown claude command field: $field" >&2
      exit 1
      ;;
  esac

  if [[ ! -f "$tsv" ]]; then
    echo "Missing canonical metadata file: $tsv" >&2
    exit 1
  fi

  # `argument_hint` may legitimately be empty in the TSV. Use a row-presence
  # check so we don't fall back to the default just because the column is blank.
  local row_present
  row_present="$(
    awk -F'\t' -v n="$name" '
      /^#/ { next }
      NF == 0 { next }
      $1 == n { print "1"; exit }
    ' "$tsv"
  )"

  local value
  value="$(
    awk -F'\t' -v n="$name" -v f="$field" '
      /^#/ { next }
      NF == 0 { next }
      $1 == n {
        if (f == "argument_hint") { print $2; exit }
        if (f == "tools")         { print $3; exit }
        if (f == "description")   { print $4; exit }
      }
    ' "$tsv"
  )"

  if [[ -z "$row_present" ]]; then
    value="$default_value"
  fi
  printf '%s\n' "$value"
}

# Renders a JSON list literal from a comma-separated string.
# Input  : "Read,Write,Edit"
# Output : ["Read", "Write", "Edit"]
csv_to_json_list() {
  local csv="$1"
  local out="["
  local first=1
  local IFS=','
  local tool
  for tool in $csv; do
    tool="$(echo "$tool" | sed -e 's/^ *//' -e 's/ *$//')"
    if [[ -z "$tool" ]]; then
      continue
    fi
    if [[ "$first" -eq 1 ]]; then
      out="${out}\"${tool}\""
      first=0
    else
      out="${out}, \"${tool}\""
    fi
  done
  out="${out}]"
  printf '%s\n' "$out"
}

###############################################################################
# Frontmatter renderers
###############################################################################

skill_frontmatter_for_claude() {
  local skill_name="$1"
  local description argument_hint allowed_tools allowed_tools_json

  description="$(lookup_claude_skill_field "$skill_name" description)"
  argument_hint="$(lookup_claude_skill_field "$skill_name" argument_hint)"
  allowed_tools="$(lookup_claude_skill_field "$skill_name" allowed_tools)"
  allowed_tools_json="$(csv_to_json_list "$allowed_tools")"

  cat <<EOF
---
name: ${skill_name}
description: ${description}
argument-hint: "${argument_hint}"
allowed-tools: ${allowed_tools_json}
disable-model-invocation: true
---

EOF
}

agent_frontmatter_for_claude() {
  local agent_name="$1"
  local description tools

  description="$(lookup_claude_agent_field "$agent_name" description)"
  tools="$(lookup_claude_agent_field "$agent_name" tools)"

  cat <<EOF
---
name: ${agent_name}
description: ${description}
tools: ${tools}
---

EOF
}

# Renders Claude slash-command frontmatter. Filename is the command name, so
# we do not emit `name:`. Omits `argument-hint:` when the TSV value is blank.
command_frontmatter_for_claude() {
  local command_name="$1"
  local description argument_hint tools

  description="$(lookup_claude_command_field "$command_name" description)"
  argument_hint="$(lookup_claude_command_field "$command_name" argument_hint)"
  tools="$(lookup_claude_command_field "$command_name" tools)"

  printf -- '---\n'
  printf 'description: %s\n' "$description"
  if [[ -n "$argument_hint" ]]; then
    printf 'argument-hint: "%s"\n' "$argument_hint"
  fi
  printf 'allowed-tools: %s\n' "$tools"
  printf -- '---\n\n'
}

skill_frontmatter_for_codex() {
  local skill_name="$1"
  local override_description="${2:-}"
  local description

  if [[ -n "$override_description" ]]; then
    description="$override_description"
  else
    # Reuse the Claude TSV description so a single edit covers both adapters.
    description="$(lookup_claude_skill_field "$skill_name" description)"
  fi

  cat <<EOF
---
name: ${skill_name}
description: >-
  ${description}
---

EOF
}

###############################################################################
# Adapter generation
###############################################################################

generate_for_adapter() {
  local adapter="$1"
  local out_root="$2"
  local part src_file dest_file base

  mkdir -p "$out_root/agents" "$out_root/commands" "$out_root/rules" "$out_root/templates"

  for part in agents commands rules templates; do
    while IFS= read -r base; do
      src_file="$INFRA_ROOT/$part/$base"
      if [[ ! -f "$src_file" ]]; then
        echo "Missing canonical file: $src_file" >&2
        exit 1
      fi

      dest_file="$out_root/$part/$base"
      sed \
        -e "s#Infrastructure/agents/#${adapter}/agents/#g" \
        -e "s#Infrastructure/commands/#${adapter}/commands/#g" \
        -e "s#Infrastructure/rules/#${adapter}/rules/#g" \
        -e "s#Infrastructure/templates/#${adapter}/templates/#g" \
        "$src_file" > "$dest_file"

      # Claude requires YAML frontmatter on every .claude/agents/<name>.md so
      # the file registers as a subagent_type. Prepend it after the path
      # rewrites so the canonical Infrastructure/agents/ files stay frontmatter-free.
      if [[ "$adapter" == ".claude" && "$part" == "agents" ]]; then
        local agent_name body_tmp
        agent_name="${base%.md}"
        body_tmp="$TMP_DIR/agent_body_$agent_name.md"
        cp "$dest_file" "$body_tmp"
        {
          agent_frontmatter_for_claude "$agent_name"
          cat "$body_tmp"
        } > "$dest_file"
      fi

      # Claude slash commands render their description, argument hint, and
      # tool allowlist from YAML frontmatter. The command still registers
      # by filename without it, but the picker shows no metadata. Same
      # canonical-stays-clean pattern as agents above.
      if [[ "$adapter" == ".claude" && "$part" == "commands" ]]; then
        local command_name body_tmp
        command_name="${base%.md}"
        body_tmp="$TMP_DIR/command_body_$command_name.md"
        cp "$dest_file" "$body_tmp"
        {
          command_frontmatter_for_claude "$command_name"
          cat "$body_tmp"
        } > "$dest_file"
      fi
    done < <(files_for_part "$part")
  done
}

# Copies one canonical skill into the adapter, choosing wrapper-only or
# full-tree copy based on whether the canonical skill is structured.
emit_one_skill() {
  local adapter="$1"
  local out_root="$2"
  local src_dir="$3"      # Infrastructure/skills or Infrastructure/style/skills
  local skill_name="$4"

  local skill_out_dir="$out_root/skills/$skill_name"
  mkdir -p "$skill_out_dir"

  local src_path
  if ! src_path="$(canonical_skill_path "$src_dir" "$skill_name")"; then
    echo "Canonical skill not found for $skill_name in $src_dir" >&2
    exit 1
  fi

  if skill_is_structured "$src_dir" "$skill_name"; then
    # Structured: copy the entire canonical subtree wholesale, then layer
    # adapter-specific frontmatter onto SKILL.md.
    rm -rf "$skill_out_dir"
    cp -R "$src_dir/$skill_name" "$skill_out_dir"
    # Strip macOS Finder metadata that may have crept into the canonical
    # source tree from local browsing. Without this, .DS_Store would
    # propagate into every adapter on every sync.
    find "$skill_out_dir" -name '.DS_Store' -delete 2>/dev/null || true
    local skill_md="$skill_out_dir/SKILL.md"
    local body_tmp="$TMP_DIR/skill_body_$skill_name.md"
    cp "$skill_md" "$body_tmp"
    if [[ "$adapter" == ".claude" ]]; then
      {
        skill_frontmatter_for_claude "$skill_name"
        cat "$body_tmp"
      } > "$skill_md"
    else
      {
        skill_frontmatter_for_codex "$skill_name"
        cat "$body_tmp"
      } > "$skill_md"
    fi
  else
    # Flat: wrap the canonical body file with adapter-specific frontmatter.
    local dest_file="$skill_out_dir/SKILL.md"
    if [[ "$adapter" == ".claude" ]]; then
      {
        skill_frontmatter_for_claude "$skill_name"
        cat "$src_path"
      } > "$dest_file"
    else
      {
        skill_frontmatter_for_codex "$skill_name"
        cat "$src_path"
      } > "$dest_file"
    fi
  fi
}

generate_infra_skills_for_adapter() {
  local adapter="$1"
  local out_root="$2"
  local skill_name

  mkdir -p "$out_root/skills"

  while IFS= read -r skill_name; do
    emit_one_skill "$adapter" "$out_root" "$INFRA_ROOT/skills" "$skill_name"
  done < <(discover_skill_names "$INFRA_ROOT/skills")
}

generate_style_skills_for_adapter() {
  local adapter="$1"
  local out_root="$2"
  local skill_name

  mkdir -p "$out_root/skills"

  while IFS= read -r skill_name; do
    emit_one_skill "$adapter" "$out_root" "$INFRA_ROOT/style/skills" "$skill_name"
  done < <(discover_skill_names "$INFRA_ROOT/style/skills")
}

generate_command_skills_for_codex_target() {
  local out_root="$1"
  local src_file dest_file skill_name base description

  mkdir -p "$out_root/skills"

  while IFS= read -r base; do
    if command_has_dedicated_skill "$base"; then
      continue
    fi

    src_file="$INFRA_ROOT/commands/$base"
    if [[ ! -f "$src_file" ]]; then
      echo "Missing canonical command file: $src_file" >&2
      exit 1
    fi

    skill_name="${base%.md}"
    description="$(extract_first_non_heading_line "$src_file")"
    mkdir -p "$out_root/skills/$skill_name"
    dest_file="$out_root/skills/$skill_name/SKILL.md"

    {
      skill_frontmatter_for_codex "$skill_name" "$description"
      cat "$src_file"
    } > "$dest_file"
  done < <(discover_markdown_basenames "$INFRA_ROOT/commands")
}

generate_codex_native_agents() {
  local out_root="$1"
  local src_file dest_file agent_name base description reasoning_effort sandbox_mode

  mkdir -p "$out_root/agents"

  while IFS= read -r base; do
    src_file="$INFRA_ROOT/agents/$base"
    if [[ ! -f "$src_file" ]]; then
      echo "Missing canonical agent file: $src_file" >&2
      exit 1
    fi

    agent_name="${base%.md}"
    description="$(lookup_codex_agent_field "$agent_name" description)"
    reasoning_effort="$(lookup_codex_agent_field "$agent_name" reasoning_effort)"
    sandbox_mode="$(lookup_codex_agent_field "$agent_name" sandbox_mode)"
    dest_file="$out_root/agents/$agent_name.toml"

    cat > "$dest_file" <<EOF
name = "$agent_name"
description = "$description"
model_reasoning_effort = "$reasoning_effort"
sandbox_mode = "$sandbox_mode"
developer_instructions = '''
Read and follow \`Infrastructure/agents/$base\` in full before acting.
Treat \`Infrastructure/\` as the source of truth. If that file points you to rules, templates, commands, style docs, or references under \`Infrastructure/\`, read those canonical files before continuing.
Prefer the canonical \`Infrastructure/\` files over generated adapter copies when both exist.
'''
EOF
  done < <(files_for_part agents)
}

###############################################################################
# Sync / check primitives
###############################################################################

copy_tree() {
  local src_root="$1"
  local dest_root="$2"
  local dir file rel_path

  mkdir -p "$dest_root"

  while IFS= read -r dir; do
    if [[ "$dir" == "$src_root" ]]; then
      continue
    fi
    rel_path="${dir#"$src_root"/}"
    mkdir -p "$dest_root/$rel_path"
  done < <(find "$src_root" -type d)

  # Exclude macOS Finder metadata so it never makes it from a generated
  # tree into a live adapter tree (or from a live adapter tree into the
  # diff comparison).
  while IFS= read -r file; do
    rel_path="${file#"$src_root"/}"
    mkdir -p "$(dirname "$dest_root/$rel_path")"
    COPYFILE_DISABLE=1 cp "$file" "$dest_root/$rel_path"
  done < <(find "$src_root" -type f ! -name '.DS_Store')
}

compare_or_sync_tree() {
  local label="$1"
  local generated_root="$2"
  local live_root="$3"

  if [[ "$MODE" == "check" ]]; then
    if [[ ! -d "$live_root" ]]; then
      echo "Missing directory: $live_root" >&2
      return 1
    fi

    if ! diff -ru "$generated_root" "$live_root" >/dev/null; then
      echo "Drift detected in $label" >&2
      diff -ru "$generated_root" "$live_root" || true
      return 1
    fi

    return 0
  fi

  if [[ -L "$live_root" ]]; then
    rm "$live_root"
  elif [[ -e "$live_root" ]]; then
    rm -rf "$live_root"
  fi
  mkdir -p "$live_root"
  copy_tree "$generated_root" "$live_root"
}

compare_or_sync_file() {
  local label="$1"
  local generated_file="$2"
  local live_file="$3"

  if [[ "$MODE" == "check" ]]; then
    if [[ ! -f "$live_file" ]]; then
      echo "Missing file: $live_file" >&2
      return 1
    fi

    if ! diff -u "$generated_file" "$live_file" >/dev/null; then
      echo "Drift detected in $label" >&2
      diff -u "$generated_file" "$live_file" || true
      return 1
    fi

    return 0
  fi

  mkdir -p "$(dirname "$live_file")"
  COPYFILE_DISABLE=1 cp "$generated_file" "$live_file"
}

compare_or_sync_adapter() {
  local adapter="$1"
  local generated_root="$2"
  local live_root="$REPO_ROOT/$adapter"
  local part live_part live_skills
  local has_diff=0

  if [[ "$MODE" == "check" ]]; then
    for part in agents commands rules templates; do
      live_part="$live_root/$part"
      if [[ ! -d "$live_part" ]]; then
        echo "Missing directory: $live_part" >&2
        has_diff=1
        continue
      fi

      if ! diff -ru "$generated_root/$part" "$live_part" >/dev/null; then
        echo "Drift detected in $adapter/$part" >&2
        diff -ru "$generated_root/$part" "$live_part" || true
        has_diff=1
      fi
    done

    live_skills="$live_root/skills"
    if [[ ! -d "$live_skills" ]]; then
      echo "Missing directory: $live_skills" >&2
      has_diff=1
    elif ! diff -ru "$generated_root/skills" "$live_skills" >/dev/null; then
      echo "Drift detected in $adapter/skills" >&2
      diff -ru "$generated_root/skills" "$live_skills" || true
      has_diff=1
    fi

    return "$has_diff"
  fi

  for part in agents commands rules templates; do
    live_part="$live_root/$part"
    if [[ -L "$live_part" ]]; then
      rm "$live_part"
    elif [[ -e "$live_part" ]]; then
      rm -rf "$live_part"
    fi
    mkdir -p "$live_part"
    copy_tree "$generated_root/$part" "$live_part"
  done

  live_skills="$live_root/skills"
  if [[ -L "$live_skills" ]]; then
    rm "$live_skills"
  elif [[ -e "$live_skills" ]]; then
    rm -rf "$live_skills"
  fi
  mkdir -p "$live_skills"
  copy_tree "$generated_root/skills" "$live_skills"
}

###############################################################################
# Main
###############################################################################

generate_for_adapter ".claude" "$TMP_DIR/.claude"
generate_for_adapter ".codex" "$TMP_DIR/.codex"

assert_unique_generated_skill_names

generate_infra_skills_for_adapter ".claude" "$TMP_DIR/.claude"
generate_infra_skills_for_adapter ".codex" "$TMP_DIR/.codex"
generate_style_skills_for_adapter ".claude" "$TMP_DIR/.claude"
generate_style_skills_for_adapter ".codex" "$TMP_DIR/.codex"

generate_command_skills_for_codex_target "$TMP_DIR/.codex"
generate_infra_skills_for_adapter ".agents" "$TMP_DIR/.agents"
generate_style_skills_for_adapter ".agents" "$TMP_DIR/.agents"
generate_command_skills_for_codex_target "$TMP_DIR/.agents"

generate_codex_native_agents "$TMP_DIR/.codex"

mkdir -p "$TMP_DIR/root"
COPYFILE_DISABLE=1 cp "$ROOT_INSTRUCTIONS/AGENTS.md"        "$TMP_DIR/root/AGENTS.md"
COPYFILE_DISABLE=1 cp "$ROOT_INSTRUCTIONS/CLAUDE.md"        "$TMP_DIR/root/CLAUDE.md"
COPYFILE_DISABLE=1 cp "$ROOT_INSTRUCTIONS/claude_README.md" "$TMP_DIR/root/claude_README.md"
COPYFILE_DISABLE=1 cp "$ROOT_INSTRUCTIONS/codex_README.md"  "$TMP_DIR/root/codex_README.md"
COPYFILE_DISABLE=1 cp "$ADAPTER_CONFIGS/claude.settings.json" "$TMP_DIR/root/claude.settings.json"
COPYFILE_DISABLE=1 cp "$ADAPTER_CONFIGS/codex.config.toml"    "$TMP_DIR/root/codex.config.toml"

check_exit=0
compare_or_sync_adapter ".claude" "$TMP_DIR/.claude" || check_exit=1
compare_or_sync_adapter ".codex" "$TMP_DIR/.codex" || check_exit=1
compare_or_sync_tree    ".agents/skills" "$TMP_DIR/.agents/skills" "$REPO_ROOT/.agents/skills" || check_exit=1
compare_or_sync_file    "AGENTS.md"             "$TMP_DIR/root/AGENTS.md"           "$REPO_ROOT/AGENTS.md" || check_exit=1
compare_or_sync_file    "CLAUDE.md"             "$TMP_DIR/root/CLAUDE.md"           "$REPO_ROOT/CLAUDE.md" || check_exit=1
compare_or_sync_file    ".claude/README.md"     "$TMP_DIR/root/claude_README.md"    "$REPO_ROOT/.claude/README.md" || check_exit=1
compare_or_sync_file    ".codex/README.md"      "$TMP_DIR/root/codex_README.md"     "$REPO_ROOT/.codex/README.md" || check_exit=1
compare_or_sync_file    ".claude/settings.json" "$TMP_DIR/root/claude.settings.json" "$REPO_ROOT/.claude/settings.json" || check_exit=1
compare_or_sync_file    ".codex/config.toml"    "$TMP_DIR/root/codex.config.toml"    "$REPO_ROOT/.codex/config.toml" || check_exit=1

if [[ "$MODE" == "check" ]]; then
  if [[ "$check_exit" -eq 0 ]]; then
    echo "No drift detected: generated adapter copies match canonical Infrastructure sources."
  fi
  exit "$check_exit"
fi

echo "Generated copies refreshed in .claude/, .codex/, .agents/skills/, CLAUDE.md, AGENTS.md, .claude/README.md, .codex/README.md, .claude/settings.json, and .codex/config.toml."
