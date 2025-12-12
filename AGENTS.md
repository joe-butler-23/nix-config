# Project Guidelines

[Project-specific context/guidelines should be added here]

# Agent Operating System (AOS)

This document defines standard operating procedures for AI Agents working within this repo.

SYSTEM ENVIRONMENT WARNING

The user has a NixOS machine with a zsh-p10k-kitty shell setup.

1. Read-only system: use `nix develop` or `nix-shell` instead of global installs
2. Git awareness: keep changes tracked so Nix sees them
3. Paths: always use absolute paths

## System Architecture

1. Memory layer (Org tasks/notes): source of truth for what to do
2. Procedural layer (skills): source of truth for how to do it
3. Self-learning: improve procedures over time

## Creating New Projects

Never create a new project manually. Use the automated initialization script:

```bash
~/documents/projects/sys-arc/scripts/sys-arc-init <project-name>
```

## Workflow

### Task Tracking

This repo uses Org-mode workflows for task tracking.

Guidelines:

- Capture meaningful work as an Org task
- Record brief progress notes and decisions in the task

### AI Planning Documents

Store ephemeral AI planning/design docs in `history/` (and optionally add `history/` to `.gitignore`).

<skills_system priority="1">

## Available Skills

<available_skills>

<skill>
<name>documentation</name>
<description>Technical documentation creation and improvement with emphasis on clarity and conciseness.</description>
<location>project</location>
</skill>

<skill>
<name>local-development</name>
<description>Troubleshooting for NixOS + ZSH + Hyprland development environment.</description>
<location>project</location>
</skill>

<skill>
<name>project-initializer</name>
<description>Guide for establishing new projects with AI tooling (hooks, skills, MCP, Nix) and optional Org-based task tracking.</description>
<location>project</location>
</skill>

<skill>
<name>self-learning</name>
<description>Continuous improvement system: detect patterns and evolve skills.</description>
<location>project</location>
</skill>

<skill>
<name>session-handoff</name>
<description>Mandatory workflow for ending coding sessions: capture remaining work, run checks, and sync changes.</description>
<location>project</location>
</skill>

<skill>
<name>skill-creator</name>
<description>Guide for creating effective skills.</description>
<location>project</location>
</skill>

</available_skills>

</skills_system>
