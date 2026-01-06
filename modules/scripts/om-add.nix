{ pkgs }:
pkgs.writeShellScriptBin "om-add" ''
  echo "Test: $1"
''