# Project guidelines

This repository manages the NixOS configuration for optimal system stability, performance, and reproducibility. All contributions and modifications must adhere to the following principles.

## 1. Modularity and Organization
- **Flake-based Structure**: Maintain a well-organized `flake.nix` and `flake.lock` for reproducible builds.
- **Module Separation**: Group related configurations into distinct Nix modules (e.g., `modules/desktop`, `modules/apps`).
- **Home Manager Integration**: Utilize Home Manager for user-specific configurations, ensuring separation from system-wide settings.

## 2. Best Practices
- **Nix Purity**: Prioritize pure functions and avoid imperative shell scripting within Nix expressions where declarative alternatives exist.
- **Immutability**: Embrace the immutable nature of NixOS. Avoid direct system modifications; all changes must be reflected in the configuration.
- **Declarative Approach**: Strive for a fully declarative system configuration.
- **Documentation first**: Nix can be a difficult and niche language. Always rely on documentation (either through web searches, or using the context7 mcp tools) rather than making guesses.

## 3. Security and Secrets Management
- **SOPS Integration**: All sensitive information (passwords, API keys) must be managed using `sops-nix` and encrypted. Never commit unencrypted secrets.
- **Least Privilege**: Configure services and applications with the minimum necessary permissions.

## 4. Documentation and Comments
- **Self-Documenting Code**: Write clear, concise Nix expressions.
- **Module Readmes**: Each significant module should have a `README.md` explaining its purpose and configurable options.
- **Inline Comments**: Use comments sparingly to explain complex logic or non-obvious choices, focusing on "why" rather than "what."

## 5. Testing and Validation
- **Local Builds**: Always ask the user to test configuration changes locally using `nixos-rebuild switch --flake .#yourhostname` before committing (user has to run because it requires sudo)
- **Linting**: Ensure Nix expressions are formatted and linted using `nix fmt`.

## 6. Version Control
- **Atomic Commits**: Each commit should represent a single, logical change.
- **Descriptive Messages**: Write clear and concise commit messages.
- **Regular Updates**: Keep `flake.lock` up-to-date with upstream `nixpkgs` or other dependencies.

# Agent Operating System (AOS)

This document defines the standard operating procedures for AI Agents working within this repo. It integrates **Memory (`beads`)** and **Procedures (skills)** into a unified, self-correcting workflow that enforces good practices and a robust workflow. The user has a Nix Os machine with a zsh-p10k-kitty shell setup. Be vigilant to the fact you are running on a Nix machine and all that entails, in particular mainitaining a clean and tracked git tree at all times (or Nix may not be aware a file exists, or the changes made to it), and the need to use nix develop or nix-shell instead of npm/pip/etc install since it is a read only system. When working in this repo, you MUST work within the following system:

## System Architecture

You are an autonomous engineer operating within a **robust workflow system**:

1.   **Memory Layer (`beads`)**: The source of truth for *what* to do. Never rely on chat history for task tracking.
2.  **Procedural Layer (skills)**: The source of truth for *how* to do it. These skills are codified procedures that should be automatically loaded when needed.
3.  **Self-learning**: You should use built-in reflection and learning cycles for process improvement.

## Workflow

What follows below is the guide to using beads and openskills:

## PilIar 1: Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Auto-syncs to JSONL for version control
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Beads Quick Start

**Check for ready work:**
```bash
bd ready --json
```

**Create new issues:**
```bash
bd create "Issue title" -t bug|feature|task -p 0-4 --json
bd create "Issue title" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**
```bash
bd update bd-42 --status in_progress --json
bd update bd-42 --priority 1 --json
```

**Complete work:**
```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit together**: Always commit the `.beads/issues.jsonl` file together with the code changes so issue state stays in sync with code state

### Auto-Sync

bd automatically syncs with git:
- Exports to `.beads/issues.jsonl` after changes (5s debounce)
- Imports from JSONL when newer (e.g., after `git pull`)
- No manual export/import needed!

### Managing AI-Generated Planning Documents

AI assistants often create planning and design documents during development:
- PLAN.md, IMPLEMENTATION.md, ARCHITECTURE.md
- DESIGN.md, CODEBASE_SUMMARY.md, INTEGRATION_PLAN.md
- TESTING_GUIDE.md, TECHNICAL_DESIGN.md, and similar files

**Best Practice: Use a dedicated directory for these ephemeral files**

**Recommended approach:**
- Create a `history/` directory in the project root
- Store ALL AI-generated planning/design docs in `history/`
- Keep the repository root clean and focused on permanent project files
- Only access `history/` when explicitly asked to review past planning

**Example .gitignore entry (optional):**
```
# AI planning documents (ephemeral)
history/
```

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
<name>beads-workflow</name>
<description>Comprehensive workflow for using beads (bd) task management system. Covers work capture, issue tracking, progress updates, and session handoff. Use when working with beads-enabled projects or when task management is needed.</description>
<location>project</location>
</skill>

<skill>
<name>claude-commands</name>
<description>Guide for creating custom slash commands for Claude Code. Use this skill when you need to extend Claude Code's capabilities with custom workflows, plugin commands, or reusable prompts defined in Markdown.</description>
<location>project</location>
</skill>

<skill>
<name>claude-subagents</name>
<description>Guide for creating and configuring custom sub-agents for Claude Code. Use this skill when you need to define specialized agents with dedicated system prompts, tool sets, and models to handle specific tasks.</description>
<location>project</location>
</skill>

<skill>
<name>documentation</name>
<description>Technical documentation creation and improvement with emphasis on clarity, conciseness, and maximum communication efficiency. Use for README files, API docs, project documentation, code comments, and any written content. Prioritizes brevity over verbosity, essential information over comprehensive detail, and immediate usefulness over exhaustive coverage.</description>
<location>project</location>
</skill>

<skill>
<name>local-development</name>
<description>This details the user's local development environment on NixOS with ZSH/P10K shell environment, running Hyprland window manager, and Kitty terminal. Helps with troubleshooting of command not found errors, script failures, path issues, environment setup, shell configuration, and desktop workflow problems. Uses progressive disclosure for detailed troubleshooting.</description>
<location>project</location>
</skill>

<skill>
<name>opencode-agents</name>
<description>Guide for creating and configuring custom OpenCode agents. Use this skill when you need to define specialized agents with specific system prompts, models, and tool permissions.</description>
<location>project</location>
</skill>

<skill>
<name>opencode-commands</name>
<description>Guide for creating and configuring custom OpenCode commands. Use this skill when you need to extend OpenCode capabilities, add custom workflows, or understand how to define commands with templates, arguments, and context injection.</description>
<location>project</location>
</skill>

<skill>
<name>project-initializer</name>
<description>Comprehensive workflow for establishing new projects with full AI AOS (Agent Operating System) toolkit. Use this skill when setting up new repositories or integrating AI tooling into existing ones. It employs an interactive "Project Architect" agent to handle cross-client configuration (Claude, Opencode, Gemini, Cline).</description>
<location>project</location>
</skill>

<skill>
<name>self-learning</name>
<description>AI continuous improvement system that detects learning opportunities and evolves skills based on usage patterns. Use when experiencing repeated patterns, skill gaps, workflow inefficiencies, error recovery situations, or when existing skills have not worked as expected and need improvement. Triggers on: iterative refinements, skill modifications, new domain requests, troubleshooting patterns, or workflow optimization opportunities.</description>
<location>project</location>
</skill>

<skill>
<name>session-handoff</name>
<description>Mandatory workflow for ending AI coding sessions. Triggers will include phrases likes 'we are done here', 'let's finish up for the day', 'that's all for today' etc. Ensures all work is properly tracked, quality gates are met, and everything is synchronized before handoff.</description>
<location>project</location>
</skill>

<skill>
<name>skill-creator</name>
<description>Guide for creating effective skills. This skill should be used when users want to create a new skill (or update an existing skill) that extends AI's capabilities with specialized knowledge, workflows, or tool integrations.</description>
<location>project</location>
</skill>

</available_skills>
<!-- SKILLS_TABLE_END -->

</skills_system>

## Pillar 3 Meta-Learning (Continuous Improvement)
**CRITICAL**: You should use built-in reflection and learning cycles for process improvement. There is a self-learning skill to enforce this. We do not want to just use the processes set out here, we want to ensure they are constantly evolving and improving.

1.  **Pattern Recognition**: When explaining same context twice, create new skill
2.  **Skill Improvement**: Update existing skills when gaps discovered
3.  **Process Evolution**: Use `skill-creator` for new workflow patterns

By following these 3 pillars, you ensure high-quality work through a robust and structured process.
