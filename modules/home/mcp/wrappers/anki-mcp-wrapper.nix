{pkgs, ...}: let
  ankiMcpWrapper = pkgs.writeShellApplication {
    name = "anki-mcp-wrapper";
    runtimeInputs = [pkgs.nodejs pkgs.nodePackages.ts-node];
    text = ''
      set -euo pipefail

      # The source code is now part of the main config repo.
      # We use the absolute path to it.
      SRC_DIR="/home/joebutler/nix-config/modules/home/mcp/repos/anki-mcp"

      cd "$SRC_DIR"

      echo "Installing dependencies from package-lock.json..." >&2
      ${pkgs.nodejs}/bin/npm ci

      # Execute the binary by running the 'build' and then the main script
      # This ensures the typescript is always compiled.
      echo "Building and starting anki-mcp-server..." >&2
      ${pkgs.nodejs}/bin/npm run build
      exec ${pkgs.nodejs}/bin/node build/index.js "$@"
    '';
  };
in {
  home.packages = [ankiMcpWrapper];
}
