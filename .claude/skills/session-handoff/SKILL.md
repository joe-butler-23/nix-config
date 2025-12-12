---
name: session-handoff
description: Mandatory workflow for ending AI coding sessions. Ensures work is captured, quality gates are met, and changes are synchronized before handoff.
---

# Session Handoff

Complete all steps before ending a coding session.

## 1) Capture Remaining Work

```bash
git status
git diff
```

Capture any follow-up work in your Org task system (project tasks, inbox, or daily note).

## 2) Quality Gates

Run project-appropriate checks (only those that apply):

```bash
nix fmt
# nix flake check
```

## 3) Synchronize

```bash
git pull --rebase
# resolve conflicts if any
git push
```

## 4) Handoff Notes

- Update any in-progress Org tasks with a short “where I left off” note.
- Record decisions/constraints in repo docs if they affect future work.
