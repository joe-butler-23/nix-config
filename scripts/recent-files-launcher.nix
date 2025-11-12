{ pkgs ? import <nixpkgs> {} }:

pkgs.writeShellScriptBin "recent-files-launcher" ''
  set -euo pipefail

  # Colors for fzf
  COLORS=(
    --color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796
    --color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6
    --color=marker:#b7bdf8,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796
  )

  # Get recent files from GTK recent files database
  get_recent_files() {
    local recent_file="$HOME/.local/share/recently-used.xbel"
    
    if [[ ! -f "$recent_file" ]]; then
      echo "No recent files database found."
      return 1
    fi
    
    # Parse XML to extract file paths and visited timestamps, sort by visited time, take top 10
    grep -E 'href="file://|visited="' "$recent_file" | \
    sed 'N;s/\n/ /' | \
    awk -F'href="|visited="' '{
      gsub(/".*/, "", $2)  # Extract file path
      gsub(/".*/, "", $3)  # Extract visited timestamp
      gsub(/^file:\/\//, "", $2)  # Remove file:// prefix
      print $3 " " $2
    }' | \
    sort -rn | \
    head -10 | \
    cut -d' ' -f2-
  }

  # Main script logic
  main() {
    # Get recent files
    files=$(get_recent_files)
    
    if [[ -z "$files" ]]; then
      echo "No recent files found."
      exit 1
    fi
    
    # Format files for fzf with filename and full path separated by tab
    formatted_files=""
    while IFS= read -r filepath; do
      [[ -z "$filepath" ]] && continue
      if [[ -f "$filepath" && -r "$filepath" ]]; then
        filename=$(basename "$filepath")
        formatted_files+="$filename"$'\t'"$filepath"$'\n'
      fi
    done <<< "$files"
    
    # Check if we have any files
    if [[ -z "$formatted_files" ]]; then
      echo "No accessible recent files found."
      exit 1
    fi
    
    # Use fzf for selection with filename display and bat preview
    selected_line=$(echo -n "$formatted_files" | ${pkgs.fzf}/bin/fzf \
      --delimiter=$'\t' \
      --layout=reverse \
      --with-nth=1 \
      --preview '${pkgs.bat}/bin/bat --color=always --style=plain --line-range :500 {2} 2>/dev/null || echo "Preview not available"' \
      --preview-window=right:60%:wrap \
      --header "Select a file to open" \
      --prompt="Recent Files > " \
      --height="80%" \
      --border=rounded \
      --pointer="▌" \
      --marker="%" \
      "''${COLORS[@]}") || exit 0
    
    # Extract the full path from the selected line
    if [[ -n "$selected_line" ]]; then
      selected_file=$(printf %s "$selected_line" | cut -d$'\t' -f2-)
      
      # Open the selected file directly
      if [[ -n "$selected_file" && -f "$selected_file" ]]; then
        mime="$(${pkgs.file}/bin/file --mime-type -Lb "$selected_file" 2>/dev/null || echo "")"
        
        open_detached() {
          setsid -f "$@" >/dev/null 2>&1 || true
        }
        
        case "$mime" in
          text/*|application/x-shellscript|application/json|application/xml)
            # Check if we should use terminal wrapper or launch GUI directly
            editor_cmd="${pkgs.mousepad}/bin/mousepad"
            editor_base="$(basename "$editor_cmd")"
            case "$editor_base" in
              nvim|vim|vi|nano|hx|helix|kak|micro)
                # Terminal editors: need terminal wrapper
                if command -v footclient >/dev/null 2>&1 && pgrep -x foot >/dev/null 2>&1; then
                  open_detached footclient -a editor -D ~ "$editor_cmd" "$selected_file"
                elif command -v foot >/dev/null 2>&1; then
                  open_detached foot -a editor -D ~ "$editor_cmd" "$selected_file"
                else
                  term="''${TERMINAL:-xterm}"
                  open_detached "$term" -e "$editor_cmd" "$selected_file"
                fi
                ;;
              *)
                # GUI editors: launch directly, no terminal wrapper
                open_detached "$editor_cmd" "$selected_file"
                ;;
            esac
            ;;
          *)
            if command -v xdg-open >/dev/null 2>&1; then
              open_detached xdg-open "$selected_file"
            else
              echo "xdg-open not found (xdg-utils). Cannot open non-text file." >&2
              exit 1
            fi
            ;;
        esac
        
        # If running inside Kitty, ask Kitty to close this window explicitly
        if command -v kitty >/dev/null 2>&1 && [ -n "''${KITTY_WINDOW_ID:-}" ]; then
          kitty @ close-window --match id:$KITTY_WINDOW_ID >/dev/null 2>&1 || true
        fi
        
        exit 0
      else
        echo "No valid file selected."
        exit 1
      fi
    fi
  }

  # Run main function
  main
''
