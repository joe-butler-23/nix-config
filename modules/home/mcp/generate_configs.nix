{
  pkgs,
  lib,
  config,
  ...
}: let
  # Read master server and client configurations
  masterServers = builtins.fromJSON (builtins.readFile ./master-servers.json);
  masterClients = builtins.fromJSON (builtins.readFile ./master-clients.json);

  servers = masterServers.servers or {};
  clients = masterClients.clients or {};

  # Function to transform MCP server definitions to OpenCode format
  generateOpenCodeConfig = servers: let
    transformServer = _name: server: {
      type = "local";
      command = [server.command] ++ (server.args or []);
      enabled = true;
      environment = server.environment or {};
    };
  in {
    "$schema" = "https://opencode.ai/config.json";
    mcp = lib.mapAttrs transformServer servers;
  };

  # Generate configurations for all clients
  generatedConfigs =
    lib.mapAttrs (
      clientName: clientConfig: let
        outputDirectory = lib.replaceStrings ["~"] [config.home.homeDirectory] clientConfig.directory;
        outputFilePath = "${outputDirectory}/${clientConfig.fileName}";

        configContent =
          if clientConfig.format == "opencode"
          then builtins.toJSON (generateOpenCodeConfig servers)
          else builtins.toJSON servers; # Direct copy for generic mcp format
      in {
        name = clientName;
        path = outputFilePath;
        content = configContent;
        mergePath = clientConfig.mergePath or null;
      }
    )
    clients;
in
  pkgs.writeShellApplication {
    name = "generate-mcp-configs";
    runtimeInputs = [pkgs.jq]; # Include jq for potential future validation/pretty printing
    text = lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        _clientName: clientConfig: let
          # Escape single quotes in configContent for shell here-string
          escapedConfigContent = lib.replaceStrings ["'"] ["'\''"] clientConfig.content;
        in ''
          mkdir -p "$(dirname ${clientConfig.path})"
          if [ -f "${clientConfig.path}" ] && [ "${clientConfig.mergePath}" != "null" ]; then
            # Merge into existing file using jq
            tmp_file=$(mktemp)
            jq --argjson mcp_content '${escapedConfigContent}' \
               --arg merge_path "${clientConfig.mergePath}" \
               'setpath($merge_path | split("."); $mcp_content)' \
               "${clientConfig.path}" > "$tmp_file" && mv "$tmp_file" "${clientConfig.path}"
            echo "Merged config for ${clientConfig.name} at ${clientConfig.path}"
          else
            # Write or overwrite the file
            echo '${escapedConfigContent}' > "${clientConfig.path}"
            echo "Generated config for ${clientConfig.name} at ${clientConfig.path}"
          fi
        ''
      )
      generatedConfigs
    );
  }
