---
name: skill-creator
description: "Trigger: new skills, agent instructions, documenting AI usage patterns. Create LLM-first skills with valid frontmatter."
license: Apache-2.0
metadata:
  author: gentleman-programming
  version: "1.0"
  scope: [root]
  auto_invoke: "Creating new skills"
allowed-tools: Read, Edit, Write, Glob, Grep, Bash, WebFetch, WebSearch, Task
---

## Activation Contract

Load this skill when creating, refactoring, or reviewing a reusable AI skill, skill instruction, or project-specific agent workflow.

## Hard Rules

- Apply `docs/skill-style-guide.md` before the inline fallback rules.
- Treat `SKILL.md` as a compact runtime contract for an LLM, not human documentation.
- Keep the skill body concise: target 180–450 tokens; hard max 1000 tokens.
- Use single-line quoted `description` with trigger words first.
- Do not add a `Keywords` section.
- Move long explanations, examples, edge cases, and background into local `references/`.
- Put templates, schemas, fixtures, and generated examples in `assets/`.
- Keep references local and relative when possible.

## Decision Gates

| Need | Action |
|---|---|
| Repeated AI workflow or convention | Create/refactor a skill. |
| One-off task or obvious rule | Do not create a skill. |
| Long prose in `SKILL.md` | Move it to `references/`. |
| Template, script, schema, fixture | Put it in `assets/`. |
| New/changed auto-invoke metadata | Run `skill-sync`. |

## Execution Steps

1. Check `docs/skill-style-guide.md`; use it as the normative source.
2. Verify the skill does not already exist and the pattern is reusable.
3. Create/update `skills/{skill-name}/SKILL.md` using the required section order.
4. Use `assets/SKILL-TEMPLATE.md` as the starting template.
5. Add local supporting detail under `references/` when the runtime body would exceed the budget.
6. Add `metadata.scope` and `metadata.auto_invoke` when the skill should update `AGENTS.md`.
7. Run `bash "./skills/skill-sync/assets/sync.sh"` after metadata changes.

## Output Contract

Return the files changed, whether the style guide was applied, any `AGENTS.md` sync required/performed, and supporting `assets/` or `references/` added.

## References

- `../../docs/skill-style-guide.md` — LLM-first skill authoring guide.
- `references/llm-first-authoring.md` — local skill-creator handoff for applying the guide.
- `assets/SKILL-TEMPLATE.md` — compact starter template.
