{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.mcp;
  
  # Transform MCP server definitions to OpenCode format
  generateOpenCodeConfig = servers: 
    let
      transformServer = name: server: {
        type = "local";
        command = server.command;
        args = server.args;
        enabled = true;
        # Add environment variables if they exist
        environment = server.environment or {};
      };
      
      transformedServers = mapAttrs transformServer servers;
    in {
      "$schema" = "https://opencode.ai/config.json";
      mcp = transformedServers;
    };
  
  # Generate configuration files for each target
  generateTargetConfigs = targets: 
    mapAttrs (name: target: {
      # Use the full target path as the attribute key
      "${target.directory}/${target.fileName}" = {
        text = if target.format == "opencode" 
          then builtins.toJSON (generateOpenCodeConfig cfg.servers)
          else builtins.toJSON cfg.servers;
        onChange = ''
          ${pkgs.jq}/bin/jq '.' "${target.directory}/${target.fileName}" > /dev/null || {
            echo "Invalid JSON generated for ${name}"
            exit 1
          }
        '';
      };
    }) targets;

in {
  options.services.mcp = {
    enable = mkEnableOption "MCP service";
    
    # Expose the generated config for other modules to use
    openCodeConfig = mkOption {
      type = types.attrs;
      internal = true;
      readOnly = true;
      default = generateOpenCodeConfig cfg.servers;
      description = "Generated OpenCode configuration";
    };
    
    servers = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          command = mkOption {
            type = types.str;
            description = "Command to run the MCP server";
          };
          args = mkOption {
            type = types.listOf types.str;
            default = [];
            description = "Arguments for the MCP server command";
          };
          environment = mkOption {
            type = types.attrsOf types.str;
            default = {};
            description = "Environment variables for the MCP server";
          };
        };
      });
      default = {};
      description = "MCP server configurations";
    };
    
    targets = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          directory = mkOption {
            type = types.str;
            description = "Directory to write the configuration file";
          };
          fileName = mkOption {
            type = types.str;
            description = "Name of the configuration file";
          };
          format = mkOption {
            type = types.enum ["mcp" "opencode"];
            default = "mcp";
            description = "Output format for the configuration";
          };
        };
      });
      default = {};
      description = "Target configurations for different tools";
    };
  };

  config = mkIf cfg.enable {
    # Ensure jq is available for JSON validation
    home.packages = [ pkgs.jq ];
    
    # Generate configuration files for all targets
    home.file = generateTargetConfigs cfg.targets;
  };
}
