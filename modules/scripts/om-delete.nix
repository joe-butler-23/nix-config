{ pkgs }:
pkgs.writeShellScriptBin "om-delete" ''
  set -euo pipefail

  MEMORY_DIR="''${XDG_DATA_HOME:-$HOME/.local/share}/openmemory"
  mkdir -p "$MEMORY_DIR"

  if [ -z "$1" ]; then
    echo "Usage: om-delete <memory-id>"
    exit1
  fi

  DB_PATH="$MEMORY_DIR/memory.sqlite"
  OM_DB_PATH="$DB_PATH" \
  ${pkgs.nodejs}/bin/node ${pkgs.openmemory-js}/libexec/om-wrapper.js delete "$1"

  echo "Memory deleted: $1"
''
