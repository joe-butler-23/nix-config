---
name: opencode-commands
description: Guide for creating and configuring custom OpenCode commands. Use this skill when you need to extend OpenCode capabilities, add custom workflows, or understand how to define commands with templates, arguments, and context injection.
---

# OpenCode Commands

This skill provides guidance on creating custom commands for OpenCode.

## Overview

OpenCode allows you to define custom commands that can invoke LLM actions with specific templates and context. Commands can be defined globally in configuration or as standalone markdown files.

## Quick Start

To create a new command, you can add it to your `.opencode/command/` directory.

1.  Create a markdown file: `.opencode/command/my-command.md`
2.  Add the definition:
    ```markdown
---
description: Brief description of command
---

[Prompt that will be sent to LLM goes here]
    ```

## Advanced Usage

Where appropriate, commands can include argument parsing, shell output injection (`!cmd`), and file context (`@file`).

### Arguments
- **`$ARGUMENTS`**: Passes all arguments provided by the user as a single string.

### Injections
- **`!command`**: Executes a shell command and injects its output.
    - Example: `!git status`
- **`@filename`**: Reads a file and injects its content.
    - Example: `@package.json`

## Best Practices

- **Clear Descriptions**: Ensure the `description` field clearly states the command's purpose, as this is what the agent uses to select it.
- **Context Injection**: Use `!git status` or `@README.md` to automatically provide relevant context to the LLM.
- **Modular Commands**: Break complex workflows into smaller, chainable commands.
