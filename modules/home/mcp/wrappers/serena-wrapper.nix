{pkgs, ...}: let
  serenaWrapper = pkgs.writeShellApplication {
    name = "serena-mcp-wrapper";
    runtimeInputs = [pkgs.uv pkgs.python312];
    text = ''
      set -euo pipefail
      # Tell uv to use the Nix-provided Python (avoid dynamic linking issues on NixOS)
      export UV_PYTHON_PREFERENCE="only-system"
      # Run the Serena MCP server directly from GitHub using uvx with explicit Python
      exec ${pkgs.uv}/bin/uvx --python ${pkgs.python312}/bin/python3 --from git+https://github.com/oraios/serena serena start-mcp-server "$@"
    '';
  };
in {
  home.packages = [serenaWrapper];
}
