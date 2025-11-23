## Based on: https://github.com/lewisflude/nix/blob/main/home/nixos/mcp.nix
{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.services.mcp;
in {
  ####
  # Options
  ####
  options.services.mcp = {
    enable = lib.mkEnableOption "declarative MCP configuration";

    # Where to write config files (e.g. Cline, Cursor, Claude, Gemini)
    targets = lib.mkOption {
      description = ''
        Targets that should receive a generated MCP JSON config.
        Each target specifies a directory and fileName where the JSON
        file should be copied as a real file (not a symlink).
      '';
      type = lib.types.attrsOf (lib.types.submodule (_: {
        options = {
          directory = lib.mkOption {
            type = lib.types.str;
            description = "Directory where the MCP JSON file will be written.";
          };
          fileName = lib.mkOption {
            type = lib.types.str;
            description = "File name for the MCP JSON file (e.g. mcp.json).";
          };
        };
      }));
      default = {};
    };

    # MCP servers definition
    servers = lib.mkOption {
      description = ''
        MCP servers available to all targets. These will appear under
        the "mcpServers" key in the generated JSON.
      '';
      type = lib.types.attrsOf (lib.types.submodule (_: {
        options = {
          command = lib.mkOption {
            type = lib.types.str;
            description = "Executable to run for this MCP server.";
          };
          args = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [];
            description = "Arguments passed to the MCP server.";
          };
          env = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            default = {};
            description = "Environment variables for this MCP server.";
          };
        };
      }));
      default = {};
    };
  };

  ####
  # Implementation
  ####
  config = lib.mkIf cfg.enable {
    # Generate one canonical JSON blob in the Nix store
    #
    # Shape:
    # {
    #   "mcpServers": {
    #     "<name>": {
    #       "command": "...",
    #       "args": [...],
    #       "env": { ... }
    #     },
    #     ...
    #   }
    # }
    #
    home.activation.mcpGenerateConfigs = lib.hm.dag.entryAfter ["writeBoundary"] (
      let
        mcpJson = builtins.toJSON {
          mcpServers =
            lib.mapAttrs (_name: server: {
              inherit (server) command args env;
            })
            cfg.servers;
        };

        mcpStoreFile = pkgs.writeText "mcp-servers.json" mcpJson;

        generateCommands = lib.concatStringsSep "\n" (
          lib.mapAttrsToList
          (
            _name: target: let
              dir = target.directory;
              path = "${target.directory}/${target.fileName}";
              escDir = lib.escapeShellArg dir;
              escPath = lib.escapeShellArg path;
            in ''
              mkdir -p ${escDir}
              # Copy from Nix store -> real file, so tools that dislike symlinks are happy
              cp ${mcpStoreFile} ${escPath}
            ''
          )
          cfg.targets
        );
      in ''
        ${generateCommands}
      ''
    );
  };
}
