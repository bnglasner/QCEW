---
description: Generate an EIG publication cover sheet from a project description. Fills known fields, infers safe defaults, and marks unresolved items as [TO FILL].
argument-hint: "<project-description>"
allowed-tools: Read,Write,Edit,Bash,Glob,Grep
---

# Skill: /cover-sheet

Generate an EIG publication cover sheet from project description text.

## Trigger

User runs `/cover-sheet <project-description>`

## Steps

1. Extract known fields from the input.
2. Infer reasonable defaults where safe.
3. Mark unresolved fields as `[TO FILL]`.
4. Use `.claude/templates/style-cover-sheet.md` for output.
5. List unresolved pre-draft questions and immediate next steps.

## Reference

- `Infrastructure/style/docs/eig-document-process.md`
