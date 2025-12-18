{pkgs}:
pkgs.writeShellScriptBin "weekly-review" ''
    export PATH="${pkgs.gum}/bin:${pkgs.fd}/bin:${pkgs.git}/bin:${pkgs.coreutils}/bin:${pkgs.systemd}/bin:${pkgs.gnugrep}/bin:${pkgs.nix-info}/bin:${pkgs.networkmanager}/bin:${pkgs.gnused}/bin:$PATH"

    # ==========================================
    # STYLING - Nord Color Palette
    # ==========================================
    NORD11="#BF616A"  # Red (errors)
    NORD13="#EBCB8B"  # Yellow (primary accent)
    NORD14="#A3BE8C"  # Green (secondary accent)
    NORD15="#B48EAD"  # Purple (success)
    NORD6="#ECEFF4"   # Bright white (main text)
    NORD3="#4C566A"   # Muted text

    # ==========================================
    # FUNCTIONS
    # ==========================================

    header() {
      clear
      gum style --foreground "$NORD6" --border double --border-foreground "$NORD13" --padding "1 2" --margin "1 0" --align center "Weekly Review & Maintenance"
    }

    update_system() {
      gum style --foreground "$NORD14" --bold --margin "1 0" "System Update"
      CONFIG_DIR="$HOME/nix-config"

      if [ ! -d "$CONFIG_DIR" ]; then
        gum style --foreground "$NORD11" "Config directory $CONFIG_DIR not found!"
        return
      fi

      cd "$CONFIG_DIR"

      # Update Flakes
      gum spin --spinner dot --title "Updating flake inputs..." -- nix flake update

      # Check for changes
      if git diff --quiet flake.lock; then
        gum style --foreground "$NORD15" "flake.lock is already up to date."
      else
        gum style --foreground "$NORD13" "flake.lock has changed."

        # Commit changes?
        if gum confirm "Commit & Push flake.lock changes?"; then
          gum spin --spinner line --title "Committing..." -- git add flake.lock
          gum spin --spinner line --title "Committing..." -- git commit -m "chore: update flake.lock"
          gum spin --spinner line --title "Pushing..." -- git push
          gum style --foreground "$NORD15" "Changes pushed."
        else
          gum style --foreground "$NORD3" "Skipped commit."
        fi

        # Apply System Updates?
        if gum confirm "Apply system updates now (sudo)?"; then
          sudo nixos-rebuild switch --flake .
          gum style --foreground "$NORD15" "System updated."
        else
           gum style --foreground "$NORD3" "Skipped system update."
        fi
      fi

      gum input --placeholder "Press Enter to continue..."
    }

    clean_files() {
      gum style --foreground "$NORD14" --bold --margin "1 0" "File Review & Cleanup"

      # Check if file-review is available
      if ! command -v file-review &> /dev/null; then
        gum style --foreground "$NORD11" "file-review command not found!"
        gum style --foreground "$NORD13" "Please rebuild your system to enable the new file review tool."
        gum input --placeholder "Press Enter to continue..."
        return
      fi

      # Launch the standalone file review tool
      file-review
    }

    generate_diagnostics() {
      cat <<EOF
  # System Diagnostics Report
  Generated: $(date '+%Y-%m-%d %H:%M:%S')
  Hostname: $(hostname)

  ## System Information
  \`\`\`
  $(nix-info -m 2>&1 || echo "Failed to get nix-info")
  \`\`\`

  ## Failed Systemd Units
  \`\`\`
  $(systemctl --failed --no-pager 2>&1 || echo "No failed units")
  \`\`\`

  ## Disk Usage
  \`\`\`
  $(df -h / /home /nix 2>&1 | grep -v Filesystem || true)
  \`\`\`

  ## Network Status
  \`\`\`
  $(nmcli device status 2>&1 || echo "NetworkManager not available")
  \`\`\`

  ## Nix Store GC Roots Summary
  \`\`\`
  $(nix-store --gc --print-roots 2>&1 | head -n 50 || echo "Could not retrieve GC roots")
  \`\`\`

  ## System Journal - Errors (Priority 3 and below)
  \`\`\`
  $(journalctl -p 3 -xb --no-pager -n 100 2>&1 || echo "Could not retrieve journal errors")
  \`\`\`

  ## System Journal - Recent Warnings (Priority 4)
  \`\`\`
  $(journalctl -p 4 -xb --no-pager -n 50 2>&1 || echo "Could not retrieve journal warnings")
  \`\`\`

  ## Boot Status
  \`\`\`
  $(systemctl status systemd-logind.service --no-pager -l 2>&1 || true)
  \`\`\`
  EOF
    }

    ai_system_review() {
      gum style --foreground "$NORD14" --bold --margin "1 0" "AI System Review"

      # Check if Claude CLI is available
      if ! command -v claude &> /dev/null; then
        gum style --foreground "$NORD11" "Claude Code CLI not found in PATH!"
        gum style --foreground "$NORD13" "Please ensure Claude Code is installed and accessible."
        gum input --placeholder "Press Enter to continue..."
        return
      fi

      # Create cache directory
      CACHE_DIR="''${XDG_CACHE_HOME:-$HOME/.cache}/weekly-review"
      mkdir -p "$CACHE_DIR"

      # Generate diagnostics file
      DIAG_FILE="$CACHE_DIR/diagnostics-$(date +%Y%m%d-%H%M%S).md"

      gum style --foreground "$NORD13" "Gathering system diagnostics..."
      generate_diagnostics > "$DIAG_FILE"

      gum style --foreground "$NORD13" "Diagnostic data collected at: $DIAG_FILE"

      gum style --foreground "$NORD13" "Opening Claude Code for system analysis..."
      gum style --foreground "$NORD3" "Diagnostic file: $DIAG_FILE"
      echo ""

      # Open Claude with the diagnostics file
      claude "$DIAG_FILE"

      # Cleanup old diagnostics (keep last 10)
      ls -t "$CACHE_DIR"/diagnostics-*.md 2>/dev/null | tail -n +11 | xargs rm -f 2>/dev/null || true

      echo ""
      gum style --foreground "$NORD15" "Claude session ended - returning to menu"
      gum input --placeholder "Press Enter to continue..."
    }

    # ==========================================
    # MAIN LOOP
    # ==========================================

    while true; do
      header

      CHOICE=$(gum choose \
        --header.foreground "$NORD14" \
        --cursor.foreground "$NORD13" \
        --selected.foreground "$NORD13" \
        "sys_update" \
        "clean_recent_files" \
        "ai_system_review" \
        "exit")

      case "$CHOICE" in
        "sys_update")
          update_system
          ;;
        "clean_recent_files")
          clean_files
          ;;
        "ai_system_review")
          ai_system_review
          ;;
        "exit")
          echo "See you next week!"
          exit 0
          ;;
      esac
    done
''
