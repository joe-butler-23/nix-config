{
  config,
  pkgs,
  ...
}: let
  context7Wrapper = pkgs.writeShellApplication {
    name = "context7-mcp-wrapper";
    runtimeInputs = [pkgs.coreutils pkgs.nodejs];
    text = ''
      set -euo pipefail
      # Read the secret from the home-manager sops location
      CONTEXT7_API_KEY="$(cat "${config.sops.secrets.CONTEXT7_API_KEY.path}")"
      export CONTEXT7_API_KEY

      # Run the context7 MCP server
      exec ${pkgs.nodejs}/bin/npx -y @upstash/context7-mcp@latest "$@"
    '';
  };
in {
  home.packages = [context7Wrapper];
}
