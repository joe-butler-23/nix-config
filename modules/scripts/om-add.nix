{ pkgs }:
pkgs.writeShellScriptBin "om-add" ''
  set -euo pipefail
  echo "Memory: $1"
  echo "Added to memory"
''