---
name: project-initializer
description: Comprehensive guide for establishing new projects with AI tooling (hooks, skills, MCP) and Nix-based environments. Includes optional Org-based task tracking setup.
---

# Project Initializer

This skill guides initializing new projects with a consistent AI tooling layout.

## Goals

- Reproducible dev environment (Nix)
- Minimal, maintainable hooks
- Skills for procedures and domain knowledge
- Optional Org-based task tracking conventions

## Suggested Layout

```
<project-root>/
├── .claude/
│   ├── hooks/
│   └── skills/
├── docs/
├── flake.nix
├── flake.lock
└── README.md
```

## Task Tracking

If requested, set expectations for where project tasks live (repo-local `docs/tasks.org` or an external org-roam/project file) and what TODO states to use.
