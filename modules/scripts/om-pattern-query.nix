{ pkgs }:
pkgs.writeShellScriptBin "om-pattern-query" ''
  set -euo pipefail

  MEMORY_DIR="''${XDG_DATA_HOME:-$HOME/.local/share}/openmemory"
  DB_PATH="$MEMORY_DIR/memory.sqlite"
  mkdir -p "$MEMORY_DIR"

  PROJECT_NAME=$(git rev-parse --show-toplevel 2>/dev/null | xargs basename || echo "global")

  node ${pkgs.openmemory-js}/libexec/openmemory-js/dist/cli.js \
    query "$@" \
    --db "$DB_PATH" \
    --project "$PROJECT_NAME" \
    --sector "procedural" \
    --json
''
