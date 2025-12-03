# Client Configuration Specifications

This document serves as the Source of Truth for configuring AI clients. The Project Architect agent uses this to generate correct configuration files.

## 1. Claude Code
*   **Configuration File**: `.claude/settings.json` (Project root)
*   **Format**: JSON
*   **MCP Definition File**: `.mcp.json` (Project root). Contains the `mcpServers` object.
*   **Key Structures in `settings.json`**:
    *   **MCP**: `enabledMcpjsonServers` (Array of strings). Must list the server names defined in `.mcp.json`. Alternatively, set `"enableAllProjectMcpServers": true`.
    *   **Permissions**: `permissions.allow` / `permissions.deny` (Array of strings).
    *   **Hooks**: `hooks` object (e.g., `UserPromptSubmit`).
*   **MCP Template**: [templates/claude-mcp.json](templates/claude-mcp.json)
*   **Settings MCP Template**: [templates/claude-settings-mcp.json](templates/claude-settings-mcp.json)
*   **Agents**: `.claude/agents/*.md` (Markdown with YAML frontmatter).
*   **Commands**: `commands/*.md` (Markdown with YAML frontmatter).
*   **Memory**: `CLAUDE.md`.

## 2. Opencode
*   **Configuration File**: `opencode.json` (Project root)
*   **Format**: JSON/JSONC
*   **Key Structures**:
    *   **Schema**: `"$schema": "https://opencode.ai/config.json"`
    *   **MCP**: `mcp` object (Map of server names to configs).
    *   **Instructions**: `instructions` (Array of file paths).
*   **MCP Template**: [templates/opencode-mcp.json](templates/opencode-mcp.json)
*   **Agents**: `.opencode/agent/*.md`.
*   **Commands**: `.opencode/command/*.md`.
*   **Plugins**: `.opencode/plugin/*.js`.

## 3. Gemini CLI
*   **Configuration File**: `.gemini/settings.json` (Project root)
*   **Format**: JSON
*   **Key Structures**:
    *   **MCP**: `mcpServers` object (Map of server names to configs).
    *   **Context**: `context.fileName` (Array, e.g., `["GEMINI.md"]`).
    *   **Filtering**: `context.fileFiltering` (`respectGitIgnore`, `respectGeminiIgnore`).
*   **MCP Template**: [templates/gemini-mcp.json](templates/gemini-mcp.json)
*   **Memory**: `GEMINI.md`.
*   **Ignore**: `.geminiignore`.

## 4. Cline
*   **Configuration File**: `.cline/mcp_settings.json` (Project-specific path).
*   **Format**: JSON
*   **Key Structures**:
    *   **MCP**: `mcpServers` object.
        ```json
        {
          "mcpServers": {
            "server-name": {
              "command": "executable",
              "args": ["arg1"],
              "env": { "VAR": "val" }
            }
          }
        }
        ```
*   **MCP Template**: [templates/cline-mcp.json](templates/cline-mcp.json)

## 5. Standard Files
*   **flake.nix**: NixOS environment definition.
