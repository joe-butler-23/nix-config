{ pkgs ? import <nixpkgs> {} }:

(pkgs.writeShellScriptBin "directory-finder" ''
  set -euo pipefail

  # Colors for fzf
  COLORS=(
    --color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796
    --color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6
    --color=marker:#b7bdf8,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796
  )

  # Use the first argument as the starting directory, or default to the current directory.
  start_dir="''${1:-$PWD}"

  # Use 'fd' if available for faster directory listing, otherwise fall back to 'find'.
  # Both commands now exclude the home directory (~).
  if command -v ${pkgs.fd}/bin/fd >/dev/null 2>&1; then
    # fd's --exclude flag ignores the specified path.
    list_cmd="${pkgs.fd}/bin/fd -t d . \"$start_dir\" --exclude \"$HOME\""
  else
    # find's -path ... -prune option prevents it from descending into the home directory.
    list_cmd="find \"$start_dir\" -path \"$HOME\" -prune -o -type d -print 2>/dev/null"
  fi

  # fzf is now called with consistent theming and options
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

  # Try to copy the absolute path to the system clipboard using various tools.
  copied=0
  if command -v ${pkgs.wl-clipboard}/bin/wl-copy >/dev/null 2>&1; then
    printf "%s" "$abs" | ${pkgs.wl-clipboard}/bin/wl-copy
    copied=1
  elif command -v ${pkgs.xclip}/bin/xclip >/dev/null 2>&1; then
    printf "%s" "$abs" | ${pkgs.xclip}/bin/xclip -selection clipboard
    copied=1
  elif command -v ${pkgs.xsel}/bin/xsel >/dev/null 2>&1; then
    printf "%s" "$abs" | ${pkgs.xsel}/bin/xsel --clipboard --input
    copied=1
  elif command -v ${pkgs.pbcopy}/bin/pbcopy >/dev/null 2>&1; then
    printf "%s" "$abs" | ${pkgs.pbcopy}/bin/pbcopy
    copied=1
  fi

  # Provide feedback to the user.
  echo "Selected: $abs"
  if [ "$copied" -eq 1 ]; then
    echo "Copied to clipboard."
  else
    echo "No clipboard tool found; path printed above."
  fi
'')
