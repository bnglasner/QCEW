# Codex and Claude Alignment Assessment

Date: 2026-04-30

## Summary

This template now targets true first-class symmetry across Claude and Codex.

The canonical source of truth remains `Infrastructure/`, but each adapter now also has its own native runtime surfaces:

- Claude:
  - `CLAUDE.md`
  - `.claude/commands/`
  - `.claude/agents/`
  - `.claude/skills/`
  - `.claude/settings.json`
- Codex:
  - `AGENTS.md`
  - `.agents/skills/`
  - `.codex/agents/*.toml`
  - `.codex/config.toml`
  - generated `.codex/{agents,commands,rules,templates,skills}/` parity copies

## What Changed

### Shared canonical layer

- `Infrastructure/` remains the only source of truth for review logic, style logic, and workflow docs.
- `Infrastructure/scripts/manage_generated_copies.sh` now treats both `.claude/` and `.codex/` as active generated adapters again.
- The sync/check script now auto-discovers canonical Markdown runtime files under `Infrastructure/agents/`, `commands/`, `rules/`, and `templates/`, which closes the prior omission risk for newly added governance files.

### Root instruction symmetry

- Added canonical root-instruction sources in `Infrastructure/root_instructions/`.
- Synced root `CLAUDE.md` and `AGENTS.md` from those canonical files.

### Workflow symmetry

- Claude keeps slash-command workflows in `.claude/commands/`.
- Codex now gets parallel reusable workflow skills in `.agents/skills/`, generated from canonical command playbooks plus canonical Infrastructure/style skills.

### Specialist-agent symmetry

- Claude keeps project agents in `.claude/agents/*.md`.
- Codex now gets native project subagent wrappers in `.codex/agents/*.toml`, generated from the canonical `Infrastructure/agents/*.md` roles.

### Config symmetry

- Added canonical adapter-config stubs in `Infrastructure/adapter_configs/`.
- Synced `.claude/settings.json` and `.codex/config.toml` from those canonical files.

## Current Capability Matrix

| Capability | Claude runtime surface | Codex runtime surface | Status |
|---|---|---|---|
| Root project instructions | `CLAUDE.md` | `AGENTS.md` | Symmetric |
| Reusable workflow entrypoints | `.claude/commands/*.md` | `.agents/skills/*/SKILL.md` | Symmetric |
| Project specialist agents | `.claude/agents/*.md` | `.codex/agents/*.toml` | Symmetric |
| Adapter config stub | `.claude/settings.json` | `.codex/config.toml` | Symmetric |
| Canonical parity mirror | `.claude/{agents,commands,rules,templates,skills}` | `.codex/{agents,commands,rules,templates,skills}` | Symmetric |

## Remaining Differences

The remaining differences are platform-native, not template bugs:

1. Claude uses project slash commands; Codex uses repo skills instead.
2. Claude project agents are Markdown files with frontmatter; Codex native subagents are TOML wrappers.
3. Claude settings are JSON; Codex config is TOML.

These are expected differences in host runtime, but the template now gives each platform a first-class, explicit path for the same kinds of work.

## Verification Targets

Verified on April 30, 2026:

- `make brain-check`
- `make maintenance-check`

Observed structure counts:

- `Infrastructure/agents`: 11 canonical Markdown files
- `Infrastructure/commands`: 13 canonical Markdown files
- `Infrastructure/rules`: 19 canonical Markdown files
- `Infrastructure/templates`: 14 canonical Markdown files
- `.claude/agents`: 11 generated Markdown files
- `.claude/commands`: 13 generated Markdown files
- `.claude/rules`: 19 generated Markdown files
- `.claude/templates`: 14 generated Markdown files
- `.codex/agents`: 11 generated Markdown files plus 11 native `.toml` wrappers
- `.codex/commands`: 13 generated Markdown files
- `.codex/rules`: 19 generated Markdown files
- `.codex/templates`: 14 generated Markdown files
- `.claude/skills`: 4 generated project skills
- `.agents/skills`: 16 generated Codex-native repo skills
- `.codex/skills`: 16 generated Codex skill mirrors

Key structure includes:

- root `CLAUDE.md`
- root `AGENTS.md`
- `.claude/commands/` plus `.claude/agents/`
- `.agents/skills/`
- `.codex/agents/*.toml`

## Bottom Line

The repo is no longer organized as "Claude first, Codex second." It now aims for:

- one canonical shared brain in `Infrastructure/`
- one first-class Claude runtime surface
- one first-class Codex runtime surface
- one maintenance loop that checks both
