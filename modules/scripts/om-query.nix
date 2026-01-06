{ pkgs }:
pkgs.writeShellScriptBin "$base" ""
  set -euo pipefail
  echo "$base: \$@"
