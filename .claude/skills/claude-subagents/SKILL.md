---
name: claude-subagents
description: Guide for creating and configuring custom sub-agents for Claude Code. Use this skill when you need to define specialized agents with dedicated system prompts, tool sets, and models to handle specific tasks.
---

# Claude Code Sub-agents

This skill provides guidance on creating custom sub-agents for Claude Code.

## Overview

Sub-agents are specialized instances of Claude configured for specific domains or tasks. They can be defined at the project level or user level and are invoked either automatically by Claude or explicitly by the user.

## Quick Start

To create a new sub-agent for your project:

1.  Create a directory named `.claude/agents/` in your project root.
2.  Create a markdown file inside it, e.g., `.claude/agents/reviewer.md`.
3.  Define the agent:
    ```markdown
    ---
    name: reviewer
    description: Specialist in code review and security analysis
    ---
    You are a code review specialist.
    Focus on security vulnerabilities and performance bottlenecks.
    ```

## Configuration

Sub-agents are defined in Markdown files with YAML frontmatter.

**File Location:**
*   **Project-specific**: `.claude/agents/*.md`
*   **User-global**: `~/.claude/agents/*.md`

**Frontmatter Options:**

| Property | Description | Example |
| :--- | :--- | :--- |
| `name` | **Required**. The handle used to invoke the agent. | `name: sql-expert` |
| `description` | **Required**. Helps Claude decide when to use this agent. | `description: Optimize SQL queries` |
| `model` | Optional. Specific model to use. | `model: sonnet` |
| `tools` | Optional. List of allowed tools. | `tools: ["Bash", "Grep"]` |
| `permissionMode` | Optional. Tool permission level. | `permissionMode: default` |
| `skills` | Optional. Auto-load specific skills. | `skills: skill1, skill2` |

## System Prompt

The body of the Markdown file serves as the **System Prompt**. Use this space to:
*   Define the agent's persona.
*   Set specific rules and constraints.
*   Provide context or knowledge relevant to the agent's specialty.

## Example

Here is an example of a specialized "SQL Optimizer" sub-agent.

**File**: `.claude/agents/sql-opt.md`

```markdown
---
name: sql-opt
description: A database specialist for analyzing and optimizing SQL queries.
model: sonnet
tools: Bash
skills: sql-analysis
---

You are an expert Database Administrator and SQL optimizer.

Your goals are:
1.  Analyze SQL queries for efficiency.
2.  Suggest indexes to improve performance.
3.  Rewrite queries to avoid N+1 problems and full table scans.

When analyzing code, always look for the `EXPLAIN` output if available or ask the user to provide it.
```

## Invocation

*   **Automatic**: Claude Code may route tasks to this agent if the description matches the user's request.
*   **Explicit**: You can invoke it directly via an interactive menu launched via the /agents command
