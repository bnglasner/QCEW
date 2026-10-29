# Infrastructure

Shared operating system for AI work in this repo.

## Purpose
- Keep core workflow and guardrails in one place used by all agent adapters.
- Keep one canonical AI brain (review + style) shared across Claude and Codex runtime surfaces.
- Generate Codex-native repo skills in `.agents/skills/` from canonical skill sources.
- Generate root adapter instruction files (`CLAUDE.md`, `AGENTS.md`) and adapter README files (`.claude/README.md`, `.codex/README.md`) from canonical sources.

## Files
- `GUARDRAILS.md`: Non-negotiable behavior and safety rules.
- `AI_WORKFLOW.md`: Session process from intake to handoff.
- `agents/`: Canonical agent specs (review + EIG style roles) used to generate adapter copies.
- `commands/`: Canonical command playbooks (review + EIG style commands) used to generate adapter copies.
- `rules/`: Shared governance plus canonical review/style rule files.
- `templates/`: Shared template files plus canonical review/style templates.
- `references/`: Canonical reusable references (including literature catalogs and source stores).
- `skills/`: Canonical non-style skills used to generate adapter skill copies. Both flat (`<name>.md`) and structured (`<name>/SKILL.md` plus helper assets) layouts are supported.
- `style/`: EIG style subsystem (docs, tokens, themes, scripts, assets, examples, and `style/skills/`).
- `root_instructions/`: Canonical sources for `CLAUDE.md`, `AGENTS.md`, `.claude/README.md`, `.codex/README.md`.
- `adapter_configs/`: Canonical adapter configuration:
  - `claude.settings.json`: Project-scoped Claude settings stub.
  - `codex.config.toml`: Project-scoped Codex configuration stub.
  - `codex_agent_metadata.tsv`: Per-agent reasoning effort, sandbox mode, and description for Codex native subagent wrappers.
  - `claude_skill_frontmatter.tsv`: Per-skill argument-hint, allowed-tools list, and description for Claude SKILL.md frontmatter.
- `scripts/manage_generated_copies.sh`: Auto-discovers canonical Markdown runtime files (flat and structured), generates `.claude/`, `.codex/`, and `.agents/skills/` copies (including Infrastructure and style skills), syncs root and adapter README files, and checks drift.
- `scripts/check_skill_parity.py`: Verifies every canonical agent/command/skill/rule/template surface is mirrored in the right adapter trees and flags orphan adapter files.
- `scripts/check_adapter_metadata_coverage.py`: Verifies the canonical agent and skill metadata TSVs cover every canonical agent and skill, with no orphan rows.
- `scripts/check_canonical_consistency.py`: Detects unintended duplication or divergence between canonical sources; allowed duplications must be declared explicitly with rationale.
- `scripts/check_internal_path_references.py`: Walks every Markdown file (root + Infrastructure + adapter trees) and validates internal path references.
- `scripts/validate_literature_catalog.py`: Validates literature catalog entries (required fields, IDs, and file paths).
- `scripts/check_catalog_staleness.py`: Flags literature entries that have not been verified recently.
- `scripts/simulate_cps_review_smoke_test.sh`: CPS-themed smoke test for adapter content and discovery logic.
- `session_logs/`: Storage folder for all recorded session logs.
- `plans/`: Saved plans for non-trivial work.
- `specs/`: Requirements specifications for complex/ambiguous tasks.
- `explorations/`: Sandbox space for early experiments.

## Update policy
- Change shared logic here first.
- Update review/style logic in canonical Infrastructure files; regenerate adapter copies (including skills, README files, and configs) with `make brain-sync`.
- Enforce no-drift with `make brain-check` and structural integrity with `make parity-check`.
- `make maintenance-check` runs the full check suite: drift, parity, metadata coverage, canonical consistency, literature, internal references, and catalog staleness.
- New canonical Markdown files added under `Infrastructure/agents/`, `commands/`, `rules/`, or `templates/` are discovered automatically during sync/check.
- Non-style skills are canonicalized under `Infrastructure/skills/`, style skills under `Infrastructure/style/skills/`, and Codex workflow-skill copies from `Infrastructure/commands/`, with generated Codex-native copies in `.agents/skills/`.
- When you add a new canonical agent, add a row to `adapter_configs/codex_agent_metadata.tsv`. When you add a new canonical skill that needs custom argument hints or tool permissions, add a row to `adapter_configs/claude_skill_frontmatter.tsv`. `check_adapter_metadata_coverage.py` will fail until you do.
- When two canonical files intentionally share a basename (for example, a template under `templates/` and its filled-in counterpart under `rules/`), declare the pair in `ALLOWED_DUPLICATIONS` inside `scripts/check_canonical_consistency.py` with a rationale.
- The generated Markdown files under `.claude/`, `.codex/`, and `.agents/skills/` remain active parity mirrors checked against the canonical `Infrastructure/` source tree.

## Adapter parity model

Claude and Codex have different native runtime surfaces. The canonical `Infrastructure/` tree is the single source of truth, and the sync script generates each adapter's surfaces from it.

| Capability | Claude surface | Codex surface | Source of truth |
|---|---|---|---|
| Root project instructions | `CLAUDE.md` | `AGENTS.md` | `Infrastructure/root_instructions/` |
| Reusable workflow entrypoints | `.claude/commands/` | `.codex/commands/` plus `.codex/skills/` plus `.agents/skills/` | `Infrastructure/commands/` |
| Specialist agents | `.claude/agents/*.md` | `.codex/agents/*.toml` plus `.codex/agents/*.md` | `Infrastructure/agents/` plus `adapter_configs/codex_agent_metadata.tsv` |
| Reusable skills | `.claude/skills/<name>/SKILL.md` | `.codex/skills/<name>/SKILL.md` plus `.agents/skills/<name>/SKILL.md` | `Infrastructure/skills/` and `Infrastructure/style/skills/` plus `adapter_configs/claude_skill_frontmatter.tsv` |
| Adapter README | `.claude/README.md` | `.codex/README.md` | `Infrastructure/root_instructions/claude_README.md` and `codex_README.md` |
| Adapter configuration | `.claude/settings.json` | `.codex/config.toml` | `Infrastructure/adapter_configs/` |

Notable asymmetry: Claude exposes review and orchestration workflows as slash commands under `.claude/commands/`, not as auto-discovered skills. Codex bundles the same workflows as both `.codex/commands/` (parity mirror) and `.codex/skills/` plus `.agents/skills/` (skill-discovery surface). This reflects the different native discovery mechanisms of each platform; the canonical workflow content is identical.
