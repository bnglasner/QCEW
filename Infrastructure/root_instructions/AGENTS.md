# Repository Instructions

## Startup Order

1. Read `PROJECT.md` first. If the title is missing, stop and ask for it before doing deeper work.
2. Read `Infrastructure/GUARDRAILS.md`.
3. Read `Infrastructure/AI_WORKFLOW.md`.
4. Review the relevant files in `Infrastructure/rules/`.
5. For style-sensitive figures, tables, or prose, use `Infrastructure/style/docs/README.md`.

## Canonical Sources

- Treat `Infrastructure/` as the source of truth for shared workflow, guardrails, review logic, and style logic.
- Update canonical files in `Infrastructure/` first, then run `make brain-sync`.
- Run `make brain-check` after changing canonical AI files.

## Codex Runtime Notes

- Repo-local Codex workflow skills live in `.agents/skills/`.
- Native Codex subagents live in `.codex/agents/*.toml`.
- Project-scoped Codex configuration and MCP settings live in `.codex/config.toml`.
- The generated Markdown copies under `.codex/{agents,commands,rules,templates,skills}/` mirror canonical `Infrastructure/` content for parity and inspection.
