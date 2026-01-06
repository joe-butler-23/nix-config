{ pkgs }:
pkgs.writeShellScriptBin "om-add" ""
  set -euo pipefail
  echo "Test: $1"
