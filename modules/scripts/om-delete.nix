{ pkgs }:
pkgs.writeShellScriptBin "om-delete" ''
  set -euo pipefail

  MEMORY_DIR="''${XDG_DATA_HOME:-$HOME/.local/share}/openmemory"
  DB_PATH="$MEMORY_DIR/memory.sqlite"
  mkdir -p "$MEMORY_DIR"

  if [ -z "$1" ]; then
    echo "Usage: om-delete <memory-id>"
    exit 1
  fi

  node ${pkgs.openmemory-js}/libexec/openmemory-js/dist/cli.js \
    delete "$1" \
    --db "$DB_PATH"

  echo "Memory deleted: $1"
''
