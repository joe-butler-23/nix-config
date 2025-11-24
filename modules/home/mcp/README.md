# MCP Configuration Management

This directory contains the new script-based system for managing MCP (Multi-Client Protocol) server and client configurations.

## Structure

- `master-servers.json`: Defines all available MCP servers.
- `master-clients.json`: Defines all clients (targets) that consume MCP configurations.
- `generate_configs.py`: Python script to generate client-specific configuration files based on the master JSON files.
- `README.md`: This documentation.

## How to Use

1.  **Define Servers:** Edit `master-servers.json` to add, modify, or remove MCP server definitions. Each server should have a `command` (which can be a wrapper script), optional `args`, and optional `environment` variables.

    Example `master-servers.json`:
    ```json
    {
      "servers": {
        "context7": {
          "command": "/path/to/context7-mcp-wrapper",
          "args": [],
          "environment": {}
        }
      }
    }
    ```

2.  **Define Clients:** Edit `master-clients.json` to add, modify, or remove client (target) definitions. Each client needs:
    *   `directory`: The absolute path where the configuration file should be written. Use `~` for the home directory.
    *   `fileName`: The name of the configuration file.
    *   `format`: The desired output format for the client (e.g., `opencode`, `mcp`).
    *   `mergePath` (optional): For clients that require merging into an existing JSON file (e.g., `gemini-cli`'s `settings.json`), specify the JSON path to merge the MCP configuration into (e.g., ".mcp").

    Example `master-clients.json`:
    ```json
    {
      "clients": {
        "opencode": {
          "directory": "~/.config/opencode",
          "fileName": "mcp.json",
          "format": "opencode"
        },
        "cline": {
          "directory": "~/.config/VSCodium/User/globalStorage/saoudrizwan.cline-nightly/settings",
          "fileName": "cline_mcp_settings.json",
          "format": "mcp"
        },
        "gemini-cli": {
          "directory": "~/.gemini",
          "fileName": "settings.json",
          "format": "opencode",
          "mergePath": ".mcp"
        }
      }
    }
    ```

3.  **Generate Configurations:** Run the `generate_configs.py` script to generate or update all client configuration files. To ensure all necessary dependencies (like `python3` and `jq`) are available, it's recommended to run it within a `nix-shell` environment:

    ```bash
    nix-shell -p python3 jq --run "python3 modules/home/mcp/generate_configs.py"
    ```

## Server Wrappers

Server commands can point to wrapper scripts (e.g., shell scripts) that handle environment setup, secret injection (e.g., using `sops` or `agenix`), or any other pre-execution logic. This allows for flexible and secure server management.

## Verification

After running the script, check the generated files in the specified client directories to ensure they are correctly formatted and contain the expected server information.

For example, to check the OpenCode configuration:

```bash
cat ~/.config/opencode/mcp.json
```

This should output the generated JSON for OpenCode.
