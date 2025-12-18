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

    gum_choose() {
      command gum choose "$@" </dev/tty
    }

    gum_confirm() {
      command gum confirm "$@" </dev/tty
    }

    gum_input() {
      command gum input "$@" </dev/tty
    }

    strip_ansi() {
      sed -r 's/\x1B\[[0-9;]*[mK]//g'
    }

    header() {
      clear
      gum style --foreground "$NORD6" --border double --border-foreground "$NORD13" --padding "1 2" --margin "1 0" --align center "Weekly Review & Maintenance"
    }

    update_system() {
      gum style --foreground "$NORD14" --bold --margin "1 0" "System Update"
      CONFIG_DIR="$HOME/nix-config"

      if [ ! -d "$CONFIG_DIR" ]; then
        gum style --foreground "$NORD11" "Config directory $CONFIG_DIR not found!"
        return 1
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
        if gum_confirm "Commit & Push flake.lock changes?"; then
          gum spin --spinner line --title "Committing..." -- git add flake.lock
          gum spin --spinner line --title "Committing..." -- git commit -m "chore: update flake.lock"
          gum spin --spinner line --title "Pushing..." -- git push
          gum style --foreground "$NORD15" "Changes pushed."
        else
          gum style --foreground "$NORD3" "Skipped commit."
        fi

        # Apply System Updates?
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

    clean_files() {
      gum style --foreground "$NORD14" --bold --margin "1 0" "File Review & Cleanup"

      # Check if file-review is available
      if ! command -v file-review &> /dev/null; then
        gum style --foreground "$NORD11" "file-review command not found!"
        gum style --foreground "$NORD13" "Please rebuild your system to enable the new file review tool."
        gum_input --placeholder "Press Enter to continue..."
        return 1
      fi

      # Launch the standalone file review tool
      file-review
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

      # Check if Claude CLI is available
      if ! command -v claude &> /dev/null; then
        gum style --foreground "$NORD11" "Claude Code CLI not found in PATH!"
        gum style --foreground "$NORD13" "Please ensure Claude Code is installed and accessible."
        gum_input --placeholder "Press Enter to continue..."
        return 1
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
      gum_input --placeholder "Press Enter to continue..."
      return 0
    }

    # ==========================================
    # CHECKLIST
    # ==========================================

    declare -a TASK_ORDER=(
      "sys_update"
      "clean_recent_files"
      "ai_system_review"
    )

    declare -A TASK_LABEL=(
      ["sys_update"]="System update"
      ["clean_recent_files"]="File review & cleanup"
      ["ai_system_review"]="AI system review"
    )

    run_task() {
      local id="$1"
      case "$id" in
        "sys_update") update_system ;;
        "clean_recent_files") clean_files ;;
        "ai_system_review") ai_system_review ;;
        *) return 1 ;;
      esac
    }

    checklist() {
      local -A done=()

      while true; do
        header
        gum style --foreground "$NORD3" "Pick a step (or run remaining); edit TASK_ORDER/TASK_LABEL to add more."
        echo ""

        local -a options=()
        local -A label_to_id=()

        local id
        for id in "''${TASK_ORDER[@]}"; do
          local label="''${TASK_LABEL[$id]}"
          label_to_id["$label"]="$id"

          if [ "''${done[$id]:-0}" -eq 1 ]; then
            options+=("$(printf "\033[32m[✓]\033[0m %s" "$label")")
          else
            options+=("$(printf "[ ] %s" "$label")")
          fi
        done

        options+=("run_remaining")
        options+=("reset_checklist")
        options+=("exit")

        local choice
        choice="$(gum_choose \
          --header.foreground "$NORD14" \
          --cursor.foreground "$NORD13" \
          --selected.foreground "$NORD13" \
          "''${options[@]}")"

        local clean_choice
        clean_choice="$(printf '%s' "$choice" | strip_ansi)"

        case "$clean_choice" in
          "run_remaining")
            for id in "''${TASK_ORDER[@]}"; do
              if [ "''${done[$id]:-0}" -eq 1 ]; then
                continue
              fi
              if run_task "$id"; then
                done["$id"]=1
              else
                gum style --foreground "$NORD11" "Step failed: ''${TASK_LABEL[$id]}"
                gum_input --placeholder "Press Enter to continue..."
                break
              fi
            done
            ;;
          "reset_checklist")
            done=()
            ;;
          "exit")
            echo "See you next week!"
            return 0
            ;;
          "[ ] "*|"[✓] "*)
            local label="''${clean_choice#"[ ] "}"
            label="''${label#"[✓] "}"

            local selected_id="''${label_to_id[$label]}"
            if [ -z "$selected_id" ]; then
              continue
            fi

            if run_task "$selected_id"; then
              done["$selected_id"]=1
            else
              gum style --foreground "$NORD11" "Step failed: $label"
              gum_input --placeholder "Press Enter to continue..."
            fi
            ;;
        esac
      done
    }

    checklist
''
