---
name: project-initializer
description: Comprehensive workflow for establishing new projects with full AI AOS (Agent Operating System) toolkit. Use this skill when setting up new repositories or integrating AI tooling into existing ones. It employs an interactive "Project Architect" agent to handle cross-client configuration (Claude, Opencode, Gemini, Cline).
---

# Project Initializer

This skill initializes a project with the standard "Agent Operating System" toolkit, supporting multiple AI clients and unified task management.

## Overview

Instead of a rigid script, this skill deploys a **Project Architect** agent to interview you and generate the correct configuration for your specific needs.

It supports:
*   **AI Clients**: Claude Code, Opencode, Gemini CLI, Cline.
*   **Tooling**: Beads (Memory), Nix (Environment), MCP (Tools).
*   **Context**: `AGENTS.md` (or `GEMINI.md` or `CLAUDE.md`) as the single source of truth.

## Usage

### Automated Architect (Recommended)

To start the interactive setup process, invoke the Project Architect:
S
```
"Please act as the Project Architect agent and initialize this repository following the setup-interview protocol."
```

### Manual Reference

If you prefer to configure manually, refer to the specifications:

*   **Client Configs**: [references/client-specs.md](references/client-specs.md) - Detailed JSON structures for all clients.
*   **Interview Protocol**: [references/setup-interview.md](references/setup-interview.md) - The checklist of what needs to be defined.

## Architecture

The initializer sets up the following structure:

```
project-root/
├── AGENTS.md                 # Core project context & rules
├── .beads/                   # Task memory database
├── flake.nix                 # Nix environment
├── .claude/                  # Claude Code config
│   ├── settings.json         # Config & MCP links
│   └── agents/               # Project-specific sub-agents
├── .opencode/                # Opencode config
│   ├── command/              # Custom commands
│   └── agent/                # Custom agents
├── .gemini/                  # Gemini config
└── mcp/                      # (Optional) Local MCP server definitions
```

## Configuration Details

### MCP Servers
The Architect will ask which MCP servers you need. It handles the complexity of formatting the MCP config correctly for each selected client (e.g., `mcpServers` object for Gemini vs `enabledMcpjsonServers` for Claude).

### Meta-Skills
The setup ensures your project is ready to use "Meta-Skills" like `skill-creator`, `opencode-commands`, etc., by creating the necessary directory placeholders.
