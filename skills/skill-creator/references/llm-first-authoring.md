# LLM-first Skill Authoring Notes

Use this reference when `SKILL.md` needs more context than the compact runtime contract should carry.

## What changed

The main skill file is now a short operational contract. It should answer only:

1. when the skill activates,
2. what rules cannot be violated,
3. how to choose between meaningful branches,
4. what steps to execute,
5. what to return,
6. where to load deeper local context if needed.

Everything else belongs in supporting files.

## Where supporting material goes

| Material | Location |
|---|---|
| Templates, schemas, scripts, fixtures, generated examples | `assets/` |
| Explanatory prose, examples, edge cases, long checklists | `references/` |
| Repo-wide skill style rule | `docs/skill-style-guide.md` |

## Required frontmatter pattern

```yaml
---
name: skill-name
description: "Trigger: essential trigger words. What this skill does."
license: Apache-2.0
metadata:
  author: gentleman-programming
  version: "1.0"
  scope: [root]
  auto_invoke:
    - "Action that should auto-invoke this skill"
---
```

Keep `description` quoted and on one physical line. Put trigger words first because tool discovery relies on frontmatter.

## Refactor workflow

1. Copy long explanatory material into `references/` before deleting it from `SKILL.md`.
2. Collapse repeated rules into concise hard rules.
3. Replace prose decision trees with a table.
4. Keep examples only if they are essential at runtime; otherwise move them to `assets/` or `references/`.
5. Run `bash "./skills/skill-sync/assets/sync.sh"` if `metadata.scope` or `metadata.auto_invoke` changed.
