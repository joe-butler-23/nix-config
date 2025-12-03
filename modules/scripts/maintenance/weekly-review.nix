{pkgs}:
pkgs.writeShellScriptBin "weekly-review" ''
  export PATH="${pkgs.gum}/bin:${pkgs.fd}/bin:${pkgs.git}/bin:${pkgs.coreutils}/bin:${pkgs.systemd}/bin:${pkgs.gnugrep}/bin:$PATH"

  # ==========================================
  # STYLING
  # ==========================================
  COLOR_PRIMARY="212"
  COLOR_SECONDARY="99"
  COLOR_TEXT="255"



  # ==========================================
  # FUNCTIONS
  # ==========================================

  header() {
    clear
    gum style --foreground $COLOR_PRIMARY --border double --border-foreground $COLOR_PRIMARY --padding "1 2" --margin "1 0" --align center "Weekly Review & Maintenance"
  }

  update_system() {
    gum style --foreground $COLOR_SECONDARY --bold --margin "1 0" "📦 System Update"
    CONFIG_DIR="$HOME/nix-config"

    if [ ! -d "$CONFIG_DIR" ]; then
      gum style --foreground 196 "❌ Config directory $CONFIG_DIR not found!"
      return
    fi

    cd "$CONFIG_DIR"

    # Update Flakes
    gum spin --spinner dot --title "Updating flake inputs..." -- nix flake update

    # Check for changes
    if git diff --quiet flake.lock; then
      gum style --foreground 76 "✅ flake.lock is already up to date."
    else
      gum style --foreground 220 "⚠️  flake.lock has changed."

      # Commit changes?
      if gum confirm "Commit & Push flake.lock changes?"; then
        gum spin --spinner line --title "Committing..." -- git add flake.lock
        gum spin --spinner line --title "Committing..." -- git commit -m "chore: update flake.lock"
        gum spin --spinner line --title "Pushing..." -- git push
        gum style --foreground 76 "✅ Changes pushed."
      else
        gum style --foreground 240 "Skipped commit."
      fi

      # Apply System Updates?
      if gum confirm "Apply system updates now (sudo)?"; then
        sudo nixos-rebuild switch --flake .
        gum style --foreground 76 "✅ System updated."
      else
         gum style --foreground 240 "Skipped system update."
      fi
    fi

    gum input --placeholder "Press Enter to continue..."
  }

  clean_files() {
    gum style --foreground $COLOR_SECONDARY --bold --margin "1 0" "🧹 Disk Janitor"

    TARGET_DIRS=()
    for dir in "development" "boox" "Documents" "Downloads" "Pictures" "utilities"; do
      if [ -d "$HOME/$dir" ]; then
        TARGET_DIRS+=("$HOME/$dir")
      fi
    done

    echo "Scanning for files changed in the last 7 days..."

    # Find files -> Select with Gum -> Delete
    # We use a temp file to store the list because gum filter outputs to stdout
    FILES_TO_DELETE=$(fd . "''${TARGET_DIRS[@]}" \
      --type f \
      --changed-within 7d \
      --exclude ".git" \
      --exclude "node_modules" \
      --exclude ".cache" \
      --exclude ".env" \
      --exclude ".venv" \
      | gum filter --no-limit --placeholder "Select files to DELETE (Tab to select, Enter to confirm)" --height 15)

    if [ -n "$FILES_TO_DELETE" ]; then
      echo "You selected:"
      echo "$FILES_TO_DELETE" | sed 's/^/  - /'

      if gum confirm "Are you sure you want to PERMANENTLY delete these files?"; then
        echo "$FILES_TO_DELETE" | xargs rm -v
        gum style --foreground 76 "✅ Files deleted."
      else
        gum style --foreground 240 "Operation cancelled."
      fi
    else
      gum style --foreground 240 "No files selected."
    fi

    gum input --placeholder "Press Enter to continue..."
  }

  check_health() {
    gum style --foreground $COLOR_SECONDARY --bold --margin "1 0" "❤️ System Health"

    FAILED_UNITS=$(systemctl --failed --no-pager)
    DISK_USAGE=$(df -h / | grep -v Filesystem)

    gum style --border normal --padding "0 1" --title "Failed Units" "$FAILED_UNITS"
    gum style --border normal --padding "0 1" --title "Disk Usage" "$DISK_USAGE"

    gum confirm "View System Logs (Errors)?" && journalctl -p 3 -xb | gum pager
  }

  garbage_collect() {
    gum style --foreground $COLOR_SECONDARY --bold --margin "1 0" "🗑️ Garbage Collection"

    if gum confirm "Run Garbage Collection (delete old generations)?"; then
       gum spin --spinner monkey --title "Collecting garbage..." -- nix-collect-garbage -d
       gum style --foreground 76 "✅ Garbage collected."
    fi

    gum input --placeholder "Press Enter to continue..."
  }

  # ==========================================
  # MAIN LOOP
  # ==========================================

  while true; do
    header

    CHOICE=$(gum choose \
      "📦 Update System" \
      "🧹 Clean Recent Files" \
      "❤️ Check Health" \
      "🗑️ Garbage Collect" \
      "👋 Exit")

    case "$CHOICE" in
      "📦 Update System")
        update_system
        ;;
      "🧹 Clean Recent Files")
        clean_files
        ;;
      "❤️ Check Health")
        check_health
        ;;
      "🗑️ Garbage Collect")
        garbage_collect
        ;;
      "👋 Exit")
        echo "See you next week!"
        exit 0
        ;;
    esac
  done
''
