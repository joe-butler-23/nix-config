{
  pkgs,
  ...
}: let
  context7Wrapper = pkgs.writeShellApplication {
    name = "context7-mcp-wrapper";
    runtimeInputs = [pkgs.coreutils pkgs.nodejs];
    text = ''
      set -euo pipefail
      # Read the secret from the system secrets location
      # We assume sops.secrets.CONTEXT7_API_KEY is defined in configuration.nix
      # and owned by the user.
      if [ -f /run/secrets/CONTEXT7_API_KEY ]; then
        CONTEXT7_API_KEY="$(cat /run/secrets/CONTEXT7_API_KEY)"
        export CONTEXT7_API_KEY
      else
        echo "Warning: /run/secrets/CONTEXT7_API_KEY not found." >&2
      fi

      # Run the context7 MCP server
      exec ${pkgs.nodejs}/bin/npx -y @upstash/context7-mcp@latest "$@"
    '';
  };
in {
  home.packages = [context7Wrapper];
}
