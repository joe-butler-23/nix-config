# modules/home/mcp/servers/context7.nix
{
  pkgs,
  ...
}: let
  # Wrapper for Context7
  context7Wrapper = pkgs.writeShellApplication {
    name = "context7-mcp-wrapper";
    runtimeInputs = [
      pkgs.nodejs
      pkgs.coreutils
    ];
    text = ''
      set -euo pipefail

      # Run Context7 MCP via npx
      exec ${pkgs.nodejs}/bin/npx -y @upstash/context7-mcp@latest "$@"
    '';
  };
in {
  services.mcp.servers.context7 = {
    command = "${context7Wrapper}/bin/context7-mcp-wrapper";
    args = [];
    # env = { };
  };
}
