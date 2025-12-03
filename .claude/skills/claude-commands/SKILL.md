---
name: claude-commands
description: Guide for creating custom slash commands for Claude Code. Use this skill when you need to extend Claude Code's capabilities with custom workflows, plugin commands, or reusable prompts defined in Markdown.
---

# Claude Code Commands

This skill provides guidance on creating custom slash commands for Claude Code.

## Overview

Claude Code allows you to define custom commands (Plugin commands) that serve as reusable prompts and workflows. These are defined as Markdown files located in a `commands/` directory (either in your project root or a plugin).

## Quick Start

To create a new command:

1.  Create a directory named `commands/` in your project root.
2.  Create a markdown file inside it, e.g., `commands/hello.md`.
3.  Define the command:
    ```markdown
    ---
    description: Say hello
    ---
    Hello Claude, please explain the code in @$1
    ```
4.  Run it with `/hello src/main.py`

## Syntax Reference

### Arguments
*   **`$ARGUMENTS`**: Inserts all arguments passed to the command as a single string.
*   **`$1`, `$2`, etc.**: Inserts specific positional arguments.

### Shell Integration (`!`)
Execute shell commands and inject their output into the prompt using `!`.
*   **Syntax**: `!git status`
*   **Requirement**: You **must** enable the Bash tool in the frontmatter using `allowed-tools`.

### File Context (`@`)
Read and inject file contents using `@`.
*   **Syntax**: `@path/to/file.ts`
*   **Dynamic**: Can be combined with arguments, e.g., `@$1`.

## Configuration (Frontmatter)

Commands use YAML frontmatter for configuration:

| Field | Description | Example |
| :--- | :--- | :--- |
| `description` | **Required**. Help text shown in the slash menu. | `description: Run tests` |
| `allowed-tools` | Required for `!cmd`. Whitelists tools/patterns. | `allowed-tools: Bash(npm test), Bash(git *)` |

## Example

Here is a comprehensive example of a command that uses arguments, shell commands, and file references.

**File**: `commands/refactor-component.md`
**Usage**: `/refactor-component UserProfile.tsx "convert to functional component"`

```markdown
---
description: Refactor a component with context from git and tests
allowed-tools: Bash(git diff), Bash(npm test)
---

# Task: Refactor Component

I need you to refactor the component located at: @$1

## Instructions
The specific refactoring goal is: $2

## Context
Here is the current git status to see if I have other pending changes:
!git diff --stat

## Verification
After you generate the code, please consider running the tests associated with this file.
The current test status is:
!npm test $1
```
