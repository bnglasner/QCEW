# Codex Adapter

Codex-facing adapter to the shared AI brain.

## Startup order
1. `../PROJECT.md`
2. If title is missing, request title and pause other work.
3. Ask for remaining `PROJECT.md` sections as optional inputs.
4. `../Infrastructure/GUARDRAILS.md`
5. `../Infrastructure/AI_WORKFLOW.md`
6. Review relevant files in `../Infrastructure/rules/`.
7. Ask for full session-summary log frequency and follow it.
8. Keep incremental decision logs current, and use `../Infrastructure/templates/session_log.md` for full logs in `../Infrastructure/session_logs/`.

## Shared brain links
- `agents/`, `commands/`, `rules/`, `templates/` are generated copies from canonical files in `../Infrastructure/`.
- Canonical review content lives in `../Infrastructure/agents/`, `../Infrastructure/commands/`, `../Infrastructure/rules/`, and `../Infrastructure/templates/`.
- Canonical style-system assets live in `../Infrastructure/style/`.
- Root Codex instructions live in `../AGENTS.md`.
- Repo-local Codex skills that current Codex discovers live in `../.agents/skills/`.
- Native Codex subagent wrappers live in `agents/*.toml`.
- `skills/` contains generated Codex skill copies derived from canonical Infrastructure skills, style skills, and command playbooks.
- `make brain-sync` and `make brain-check` maintain both the generated `.codex/` tree and the native Codex runtime surfaces used in this repo.

## Path-rewriting policy
- Files under `agents/`, `commands/`, `rules/`, and `templates/` are sed-rewritten so that internal references point at adapter-local copies (`.codex/...`).
- Files under `skills/` are intentionally left referencing canonical `Infrastructure/...` paths so that skill bundles remain valid when discovered by clients that may not have the full adapter mirror loaded.
- Both surfaces resolve at runtime because canonical and adapter trees both exist in this repo. The asymmetry is intentional, not a bug.

## Adapter rules
- Treat `../Infrastructure/` as source of truth for core workflow and guardrails.
- Use canonical review files in `../Infrastructure/` as source of truth for review workflows.
- Evidence standard is enforced through `../Infrastructure/GUARDRAILS.md` (no separate slash command or agent needed).
- Do not fork review logic in `.codex/`; update canonical files in `../Infrastructure/` and regenerate.
- Current Codex loads project instructions from `../AGENTS.md`, repo-local skills from `../.agents/skills/`, native subagents from `agents/*.toml`, and project config from `config.toml`.
- The generated Markdown files under `.codex/{agents,commands,rules,templates,skills}/` remain checked parity copies of the canonical Infrastructure source tree.
