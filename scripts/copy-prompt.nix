{pkgs ? import <nixpkgs> {}}: (pkgs.writeShellScriptBin "copy-prompt" ''
  set -euo pipefail

  PROMPT_DIR="/home/joebutler/Documents/prompting"

  # Colors for fzf
  COLORS=(
    --color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796
    --color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6
    --color=marker:#b7bdf8,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796
  )

  # Prefer wl-copy on Wayland; fall back to xclip if available
  CLIP_CMD=""
  if command -v ${pkgs.wl-clipboard}/bin/wl-copy >/dev/null 2>&1; then
    CLIP_CMD="${pkgs.wl-clipboard}/bin/wl-copy"
  elif command -v ${pkgs.xclip}/bin/xclip >/dev/null 2>&1; then
    CLIP_CMD="${pkgs.xclip}/bin/xclip -selection clipboard"
  else
    echo "Error: neither 'wl-copy' nor 'xclip' is available." >&2
    exit 1
  fi

  # Main script logic
  main() {
    selected_line=$(find "$PROMPT_DIR" -type f \( -name "*.txt" -o -name "*.md" \) -printf "%f\t%p\n" | \
      ${pkgs.fzf}/bin/fzf \
        --delimiter=$'\t' \
        --layout=reverse \
        --with-nth=1 \
        --preview '${pkgs.bat}/bin/bat --color=always --style=plain --line-range :500 {2}' \
        --preview-window=right:60%:wrap \
        --header "Select a prompt file. Enter to copy to clipboard." \
        --prompt="Prompt Library > " \
        --height="80%" \
        --border=rounded \
        --pointer="▌" \
        --marker="%" \
        "''${COLORS[@]}")

    # If fzf was cancelled or nothing selected, exit
    if [ $? -ne 0 ] || [ -z "''${selected_line:-}" ]; then
      exit 0
    fi

    full_path=$(printf %s "$selected_line" | cut -d$'\t' -f2)

    if [[ -n "$full_path" && -f "$full_path" ]]; then
      if [[ "$CLIP_CMD" == *"wl-copy"* ]]; then
        cat "$full_path" | $CLIP_CMD
      else
        cat "$full_path" | $CLIP_CMD
      fi
      sleep 0.1
      echo "Prompt copied to clipboard!"
    else
      echo "Error: Invalid file selection." >&2
      exit 1
    fi
  }

  # Run main function
  main
'')
