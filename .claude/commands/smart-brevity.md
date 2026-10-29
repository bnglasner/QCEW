---
description: Rewrite text using Smart Brevity principles while keeping EIG writing conventions. Returns the rewrite plus a short change log explaining what was tightened and why.
argument-hint: "<text>"
allowed-tools: Read,Write,Edit,Bash,Glob,Grep
---

# Skill: /smart-brevity

Rewrite text using Smart Brevity principles while keeping EIG writing conventions.

## Trigger

User runs `/smart-brevity <text>`

## Steps

1. Identify the core finding/news angle.
2. Rewrite with:
   - one-sentence lede,
   - explicit `Why it matters`,
   - bullets for 3+ item lists,
   - active voice and strong verbs.
3. Apply `.claude/rules/style-writing-rules.md`.
4. Return output using `.claude/templates/smart-brevity-output.md`.

## Required Add-On

Include a short change log explaining what was tightened and why.
