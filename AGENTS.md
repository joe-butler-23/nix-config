# Project: nix-config

## External File Loading
CRITICAL: When you encounter a file reference (e.g., @modules/file.md), use your Read tool to load it when relevant to the task.

Instructions:
- Load references based on actual need
- Treat loaded content as mandatory instructions
- Follow references recursively when needed

## Critical Imports
@~/projects/sys-arc/ai/tooling/file-imports/agents-header.md

## Project Overview

NixOS system configuration managed declaratively with Home Manager. This repository contains system-level configuration, dotfiles, custom scripts, and editor setup.

**Environment:**
- NixOS with Hyprland window manager
- ZSH with Powerlevel10k prompt
- Kitty terminal
- Read-only system (use `nix develop` or `nix-shell` instead of global installs)
- Git-tracked changes required for Nix awareness

**Key Directories:**
- `modules/editor/`
- `modules/scripts/` - Custom Nix-based scripts (see [DEVELOPMENT.md](modules/scripts/DEVELOPMENT.md) for script creation guide)

**Task Tracking:** This project uses **org-mode** for task management.

## Code Comments Style

**Present-State Documentation:**
- Comments document current state, NOT historical changes
- Avoid: "now", "moved to", "no longer", or any progression language
- Git history captures changes; code comments capture current reality

**Conciseness:**
- One line when possible
- No redundant explanations

**Examples:**

❌ Bad:
```nix
# Note: Kanshi service is now system-level (modules/services/kanshi.nix)
# Config is managed by chezmoi at ~/.config/kanshi/config
```

✅ Good:
```nix
# Kanshi: laptop-specific, config in chezmoi
```

## Available Skills (Progressive Disclosure)

<skills_system priority="1">

## Available Skills

<!-- SKILLS_TABLE_START -->
<usage>
When users ask you to perform tasks, check if any of the available skills below can help complete the task more effectively. Skills provide specialized capabilities and domain knowledge.

How to use skills:
- Invoke: Bash("openskills read <skill-name>")
- The skill content will load with detailed instructions on how to complete the task
- Base directory provided in output for resolving bundled resources (references/, scripts/, assets/)

Usage notes:
- Only use skills listed in <available_skills> below
- Do not invoke a skill that is already loaded in your context
- Each skill invocation is stateless
</usage>

<available_skills>

<skill>
<name>create-AGENTSmd</name>
<description>Guide for creating the AGENTS.md context file. You MUST use this skill when setting up a new project or migrating an existing one to the standard AI context architecture and a new AGENTS.md is required,</description>
<location>global</location>
</skill>

<skill>
<name>create-commands</name>
<description>Guide for creating global, cross-platform commands (Gemini, Claude, OpenCode). You MUST use this skill when asked to create a new command, reusable prompt, or workflow for AI agents.</description>
<location>global</location>
</skill>

<skill>
<name>create-skill</name>
<description>Guide for creating effective skills. This skill MUST be used when users want to create a new skill (or update an existing skill) that extends AI's capabilities with specialized knowledge, workflows, or tool integrations.</description>
<location>global</location>
</skill>

<skill>
<name>documentation</name>
<description>Technical documentation creation and improvement with emphasis on clarity, conciseness, and maximum communication efficiency. You MUST use for any technical documentation such as README files, API docs, project documentation, code comments, and any written content.</description>
<location>global</location>
</skill>

<skill>
<name>executing-plans</name>
<description>Guide to effective task execution. You MUST activate this skill when a user asks you to complete a pre-determined task, i.e. from tasks.org</description>
<location>global</location>
</skill>

<skill>
<name>gum-shell-scripts</name>
<description>Create interactive shell scripts using Charm's gum library. You MUST use when building command-line interfaces, user prompts, selection menus, or any interactive terminal UI.</description>
<location>global</location>
</skill>

<skill>
<name>local-development</name>
<description>NixOS development environment guide. MUST use when engaged in coding work.</description>
<location>global</location>
</skill>

<skill>
<name>task-refinement</name>
<description>Mandatory workflow for refining vaguely defined tasks.</description>
<location>global</location>
</skill>

</available_skills>
<!-- SKILLS_TABLE_END -->

</skills_system>
