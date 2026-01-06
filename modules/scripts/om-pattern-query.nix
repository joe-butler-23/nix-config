{ pkgs }:
pkgs.writeScript ""om-pattern-query"" ''
  #!/bin/sh
  set -euo pipefail

  MEMORY_DIR="''${XDG_DATA_HOME:-$HOME/.local/share}/openmemory"
  mkdir -p "$MEMORY_DIR"

  PROJECT_NAME=$(git rev-parse --show-toplevel 2>/dev/null | xargs basename || echo "global")
  DB_PATH="$MEMORY_DIR/memory.sqlite"

  export OM_DB_PATH="$DB_PATH"
  export OM_PROJECT="$PROJECT_NAME"
  export OPENAI_API_KEY="''${OPENAI_API_KEY}"

  # Simple test for now
  echo "Memory: $1"
  echo "DB: $DB_PATH"
  echo "Project: $PROJECT_NAME"
''