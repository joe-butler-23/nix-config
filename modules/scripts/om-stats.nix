{ pkgs }:
pkgs.writeShellScriptBin "om-stats" ''
  set -euo pipefail

  MEMORY_DIR="''${XDG_DATA_HOME:-$HOME/.local/share}/openmemory"
  DB_PATH="$MEMORY_DIR/memory.sqlite"
  mkdir -p "$MEMORY_DIR"

  PROJECT_NAME=$(git rev-parse --show-toplevel 2>/dev/null | xargs basename || echo "global")

  echo "OpenMemory Statistics"
  echo "===================="
  echo "Database: $DB_PATH"
  echo "Project: $PROJECT_NAME"

  MEMORY_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM memories;" 2>/dev/null || echo "N/A")
  echo "Total Memories: $MEMORY_COUNT"

  LATEST=$(sqlite3 "$DB_PATH" "SELECT created_at FROM memories ORDER BY created_at DESC LIMIT 1;" 2>/dev/null || echo "N/A")
  echo "Latest Memory: $LATEST"

  DB_SIZE=$(du -h "$DB_PATH" 2>/dev/null | cut -f1 || echo "N/A")
  echo "Database Size: $DB_SIZE"
''
