{pkgs}:
pkgs.writeShellScriptBin "system-maintenance" ''
        export PATH="${pkgs.gum}/bin:${pkgs.fd}/bin:${pkgs.git}/bin:${pkgs.coreutils}/bin:${pkgs.systemd}/bin:${pkgs.gnugrep}/bin:${pkgs.nix-info}/bin:${pkgs.networkmanager}/bin:${pkgs.gnused}/bin:$PATH"

        NORD11="#BF616A"
        NORD13="#EBCB8B"
        NORD14="#A3BE8C"
        NORD15="#B48EAD"
        NORD6="#ECEFF4"
        NORD3="#4C566A"

        gum_choose() {
          command gum choose "$@" </dev/tty
        }

        gum_confirm() {
          command gum confirm "$@" </dev/tty
        }

        gum_input() {
          command gum input "$@" </dev/tty
        }

        header() {
          clear
          gum style --foreground "$NORD6" --border double --border-foreground "$NORD13" --padding "1 2" --margin "1 0" --align center "System Maintenance"
        }

        update_system() {
          gum style --foreground "$NORD14" --bold --margin "1 0" "System Update"
          CONFIG_DIR="$HOME/nix-config"

          if [ ! -d "$CONFIG_DIR" ]; then
            gum style --foreground "$NORD11" "Config directory $CONFIG_DIR not found!"
            gum_input --placeholder "Press Enter to continue..."
            return 1
          fi

          cd "$CONFIG_DIR"

          gum spin --spinner dot --title "Updating flake inputs..." -- nix flake update

          if git diff --quiet flake.lock; then
            gum style --foreground "$NORD15" "flake.lock is already up to date."
          else
            gum style --foreground "$NORD13" "flake.lock has changed."

            if gum_confirm "Commit & Push flake.lock changes?"; then
              gum spin --spinner line --title "Committing..." -- git add flake.lock
              gum spin --spinner line --title "Committing..." -- git commit -m "chore: update flake.lock"
              gum spin --spinner line --title "Pushing..." -- git push
              gum style --foreground "$NORD15" "Changes pushed."
            else
              gum style --foreground "$NORD3" "Skipped commit."
            fi

            if gum_confirm "Apply system updates now (sudo)?"; then
              sudo nixos-rebuild switch --flake .
              gum style --foreground "$NORD15" "System updated."
            else
              gum style --foreground "$NORD3" "Skipped system update."
            fi
          fi

          gum_input --placeholder "Press Enter to continue..."
          return 0
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

          if ! command -v claude &> /dev/null; then
            gum style --foreground "$NORD11" "Claude Code CLI not found in PATH!"
            gum style --foreground "$NORD13" "Please ensure Claude Code is installed and accessible."
            gum_input --placeholder "Press Enter to continue..."
            return 1
          fi

          CACHE_DIR="''${XDG_CACHE_HOME:-$HOME/.cache}/weekly-review"
          mkdir -p "$CACHE_DIR"

          DIAG_FILE="$CACHE_DIR/diagnostics-$(date +%Y%m%d-%H%M%S).md"

          gum style --foreground "$NORD13" "Gathering system diagnostics..."
          generate_diagnostics > "$DIAG_FILE"

          gum style --foreground "$NORD13" "Diagnostic data collected at: $DIAG_FILE"
          gum style --foreground "$NORD13" "Opening Claude Code for system analysis..."
          gum style --foreground "$NORD3" "Diagnostic file: $DIAG_FILE"
          echo ""

          claude "$DIAG_FILE"

          ls -t "$CACHE_DIR"/diagnostics-*.md 2>/dev/null | tail -n +11 | xargs rm -f 2>/dev/null || true

          echo ""
          gum style --foreground "$NORD15" "Claude session ended - returning to menu"
          gum_input --placeholder "Press Enter to continue..."
          return 0
        }

      while true; do
        header

        choice=""
        if ! choice="$(gum_choose \
          --header.foreground "$NORD14" \
          --cursor.foreground "$NORD13" \
          --selected.foreground "$NORD13" \
          "update_system" \
          "ai_system_review" \
          "back")"; then
          exit 0
        fi

          case "$choice" in
            "update_system") update_system ;;
            "ai_system_review") ai_system_review ;;
            "back") exit 0 ;;
          esac
        done
''
