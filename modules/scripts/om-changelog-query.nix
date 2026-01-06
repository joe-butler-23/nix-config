{ pkgs }:
pkgs.writeShellScriptBin "om-changelog-query" ''
  set -euo pipefail

  MEMORY_DIR="''${XDG_DATA_HOME:-$HOME/.local/share}/openmemory"
  mkdir -p "$MEMORY_DIR"

  PROJECT_NAME=$(git rev-parse --show-toplevel 2>/dev/null | xargs basename || echo "global")
  DB_PATH="$MEMORY_DIR/memory.sqlite"

  OM_DB_PATH="$DB_PATH" \
  ${pkgs.openmemory-js}/bin/opm query "$@"
''
