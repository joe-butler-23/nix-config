---
name: opencode-agents
description: Guide for creating and configuring custom OpenCode agents. Use this skill when you need to define specialized agents with specific system prompts, models, and tool permissions.
---

# OpenCode Agents

This skill provides guidance on creating custom agents for OpenCode.

## Overview

OpenCode agents are specialized AI assistants configured for specific tasks. They can be defined globally in your `opencode.json` configuration or as standalone Markdown files. We prefer markdown files wherever possible.

## Quick Start

To create a new agent, you can add it to your `.opencode/agent/` directory in the project repo.

1.  Create a markdown file: `.opencode/agent/my-agent.md`
2.  Add the definition:
    ```markdown
    ---
    description: Explain what the agent is for
    ---

    Your system prompt goes here.
    You can include instructions on how the agent should behave.
    ```

## Configuration

Agents are configured using YAML frontmatter (in Markdown files) or JSON properties (in `opencode.json`).

| Property | Type | Description |
| :--- | :--- | :--- |
| `description` | string | **Required**. A brief explanation of the agent's specialization. |
| `model` | string | Optional. The specific LLM to use (e.g., `anthropic/claude-4-5-sonnet-20240620`). |
| `temperature` | number | Optional. Controls randomness (0.0 to 1.0). |
| `tools` | array | Optional. List of tools the agent is allowed to use. |
| `mode` | string | Optional. `primary` (default) or `subagent`. Subagents are optimized for being called by other agents. |

## System Prompt

The body of the Markdown file (or the `prompt` field in JSON) serves as the **System Prompt**. This is where you define the agent's persona, constraints, and core instructions.

## Example

Here is an example of a specialized "Code Reviewer" agent.

**File**: `.opencode/agent/code-reviewer.md`

```markdown
---
description: A strict code reviewer that focuses on security and performance.
model: anthropic/claude-4-5-opus
temperature: 0.2
mode: subagent
tools:
  - read_file
  - search_code
---

You are a senior code reviewer with a focus on security and performance optimization.

Your responsibilities:
1.  Analyze code for potential security vulnerabilities (SQL injection, XSS, etc.).
2.  Identify performance bottlenecks (N+1 queries, inefficient loops).
3.  Ensure code adheres to SOLID principles.

Do NOT rewrite the code unless explicitly asked. Focus on providing actionable feedback.
```
