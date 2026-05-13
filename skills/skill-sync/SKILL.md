---
name: skill-sync
description: "Trigger: skill-sync, actualizar Auto-invoke, metadata.scope, metadata.auto_invoke. Sincroniza skills con AGENTS.md."
license: Apache-2.0
metadata:
  author: gentleman-programming
  version: "1.0"
  scope: [root]
  auto_invoke:
    - "After creating/modifying a skill"
    - "Regenerate AGENTS.md Auto-invoke tables (sync.sh)"
    - "Troubleshoot why a skill is missing from AGENTS.md auto-invoke"
allowed-tools: Read, Edit, Write, Glob, Grep, Bash
---

## Activation Contract

Load this skill after creating/modifying a skill, changing `metadata.scope` or `metadata.auto_invoke`, or regenerating Auto-invoke tables in `AGENTS.md`.

## Hard Rules

- Only skills with `metadata.scope` and `metadata.auto_invoke` are included.
- Preserve `## Skills Reference` and `### Auto-invoke Skills` anchors in `AGENTS.md`.
- Run dry-run first when checking expected changes.
- Run the real sync after metadata changes that should affect auto-invoke tables.
- Verify generated rows reflect the intended trigger actions.

## Decision Gates

| Case | Action |
|---|---|
| Creating/modifying skill metadata | Run sync. |
| Unsure what changes | Run `--dry-run`. |
| Scope-specific check | Use `--scope <scope>`. |
| Skill missing from AGENTS.md | Check `metadata.scope` and `metadata.auto_invoke`. |

## Execution Steps

1. Inspect changed skill frontmatter.
2. Run `bash "./skills/skill-sync/assets/sync.sh" --dry-run` if review is needed.
3. Run `bash "./skills/skill-sync/assets/sync.sh"` to update tables.
4. Review `AGENTS.md` diff and confirm expected rows.

## Output Contract

Return sync command used, files changed, missing metadata issues, and any AGENTS.md rows added/removed.

## References

- `assets/sync.sh` — sync implementation.
- `assets/sync_test.sh` — test coverage for the sync script.
