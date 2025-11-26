{pkgs ? import <nixpkgs> {}}: (pkgs.writeShellScriptBin "directory-finder" ''
  set -euo pipefail

  # Colors for fzf
  COLORS=(
    --color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796
    --color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6
    --color=marker:#b7bdf8,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796
  )

  # Use the first argument as the starting directory, or default to the current directory.
  start_dir="''${1:-$PWD}"

  # Use fd for directory listing, excluding home directory
  list_cmd="${pkgs.fd}/bin/fd -t d . \"$start_dir\" --exclude \"$HOME\""

  # fzf with consistent theming and options
  choice=$(${pkgs.fzf}/bin/fzf \
    --prompt="📁 Directory > " \
    --height="80%" \
    --reverse \
    --select-1 \
    --exit-0 \
    --no-info \
    --border=rounded \
    --pointer="▌" \
    --marker="%" \
    "''${COLORS[@]}" < <(eval "$list_cmd"))

  # Exit if no choice was made (e.g., user pressed Esc).
  [ -z "$choice" ] && exit 1

  # Get the absolute path of the selected directory.
  abs=$(${pkgs.coreutils}/bin/realpath "$choice")

  # Copy to clipboard using wl-copy
  printf "%s" "$abs" | ${pkgs.wl-clipboard}/bin/wl-copy

  # Provide feedback to the user.
  echo "Selected: $abs"
  echo "Copied to clipboard."
'')
