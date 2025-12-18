{pkgs}:
pkgs.writeShellScriptBin "file-review" ''
  export PATH="${pkgs.gum}/bin:${pkgs.fd}/bin:${pkgs.coreutils}/bin:${pkgs.trash-cli}/bin:${pkgs.gnugrep}/bin:${pkgs.findutils}/bin:${pkgs.git}/bin:${pkgs.gnused}/bin:$PATH"

  # ==========================================
  # CONFIGURATION
  # ==========================================

  # Thresholds
  LARGE_FILE_SIZE="100M"
  RECENT_DAYS=7
  ABANDONED_DAYS=30

  # Target directories for scanning
  TARGET_DIRS=(
    "$HOME/Downloads"
    "$HOME/development"
    "$HOME/projects"
    "$HOME/Documents"
    "$HOME/Pictures"
  )

  # Development artifacts patterns
  ARTIFACTS=(
    "node_modules"
    ".venv"
    "venv"
    "__pycache__"
    ".pytest_cache"
    "target"  # Rust
    "dist"
    "build"
    ".cache"
    "*.pyc"
    ".DS_Store"
    "Thumbs.db"
  )

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
  # STATE TRACKING
  # ==========================================
  TRASH_COUNT=0
  TRASH_SIZE=0
  KEEP_COUNT=0
  SESSION_LOG="$HOME/.cache/file-review/session-$(date +%Y%m%d-%H%M%S).log"
  mkdir -p "$(dirname "$SESSION_LOG")"

  log_action() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$SESSION_LOG"
  }

  # ==========================================
  # UTILITY FUNCTIONS
  # ==========================================

  header() {
    clear
    gum style --foreground "$NORD6" --border double --border-foreground "$NORD13" --padding "1 2" --margin "1 0" --align center "Weekly File Review"
  }

  get_human_size() {
    local path="$1"
    if [ -f "$path" ]; then
      du -sh "$path" 2>/dev/null | cut -f1
    elif [ -d "$path" ]; then
      du -sh "$path" 2>/dev/null | cut -f1
    else
      echo "0"
    fi
  }

  get_size_bytes() {
    local path="$1"
    if [ -f "$path" ]; then
      stat -c%s "$path" 2>/dev/null || echo "0"
    elif [ -d "$path" ]; then
      du -sb "$path" 2>/dev/null | cut -f1 || echo "0"
    else
      echo "0"
    fi
  }

  review_item() {
    local item="$1"
    local context="$2"

    if [ ! -e "$item" ]; then
      return
    fi

    local size=$(get_human_size "$item")
    local size_bytes=$(get_size_bytes "$item")

    header
    gum style --foreground "$NORD14" --bold "Reviewing: $context"
    echo ""
    gum style --foreground "$NORD13" "Path: $item"
    gum style --foreground "$NORD3" "Size: $size"

    # Show extra context if it's a directory
    if [ -d "$item" ]; then
      local file_count=$(find "$item" -type f 2>/dev/null | wc -l)
      gum style --foreground "$NORD3" "Files: $file_count"

      # Check if it's a git repo
      if [ -d "$item/.git" ]; then
        local last_commit=$(cd "$item" && git log -1 --format="%ar" 2>/dev/null || echo "unknown")
        gum style --foreground "$NORD3" "Last commit: $last_commit"
      fi
    fi

    echo ""
    local action=$(gum choose --header.foreground "$NORD14" \
      "trash" \
      "keep" \
      "preview" \
      "skip_remaining")

    case "$action" in
      "trash")
        if trash-put "$item" 2>/dev/null; then
          gum style --foreground "$NORD15" "✓ Moved to trash"
          log_action "TRASH: $item ($size)"
          TRASH_COUNT=$((TRASH_COUNT + 1))
          TRASH_SIZE=$((TRASH_SIZE + size_bytes))
        else
          gum style --foreground "$NORD11" "✗ Failed to trash"
          log_action "ERROR: Failed to trash $item"
        fi
        sleep 0.5
        ;;
      "keep")
        gum style --foreground "$NORD3" "Kept"
        log_action "KEEP: $item"
        KEEP_COUNT=$((KEEP_COUNT + 1))
        sleep 0.3
        ;;
      "preview")
        if [ -d "$item" ]; then
          gum style --foreground "$NORD13" "Directory contents:"
          ls -lah "$item" | head -n 20
        else
          gum style --foreground "$NORD13" "File preview:"
          file "$item"
          head -n 20 "$item" 2>/dev/null || echo "(binary file)"
        fi
        gum input --placeholder "Press Enter to continue..."
        review_item "$item" "$context"  # Show choices again
        ;;
      "skip_remaining")
        return 1
        ;;
    esac

    return 0
  }

  # ==========================================
  # CATEGORY SCANNERS
  # ==========================================

  review_recent_downloads() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Recent Downloads (< $RECENT_DAYS days)"

    if [ ! -d "$HOME/Downloads" ]; then
      gum style --foreground "$NORD3" "Downloads directory not found"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    gum spin --spinner dot --title "Scanning Downloads..." -- sleep 1

    local files=$(find "$HOME/Downloads" -type f -mtime -$RECENT_DAYS 2>/dev/null | sort)
    local count=$(echo "$files" | grep -c . || echo "0")

    if [ "$count" -eq 0 ] || [ -z "$files" ]; then
      gum style --foreground "$NORD15" "✓ No recent downloads to review"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    gum style --foreground "$NORD13" "Found $count recent files"
    echo ""

    if ! gum confirm "Review these files?"; then
      return
    fi

    while IFS= read -r file; do
      [ -z "$file" ] && continue
      review_item "$file" "Recent Download" || break
    done <<< "$files"
  }

  review_large_files() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Large Files (> 100MB)"

    gum spin --spinner dot --title "Scanning for large files..." -- sleep 1

    local existing_dirs=()
    for dir in "''${TARGET_DIRS[@]}"; do
      [ -d "$dir" ] && existing_dirs+=("$dir")
    done

    if [ ''${#existing_dirs[@]} -eq 0 ]; then
      gum style --foreground "$NORD3" "No target directories found"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    local files=$(find "''${existing_dirs[@]}" -type f -size +$LARGE_FILE_SIZE 2>/dev/null | sort -h)
    local count=$(echo "$files" | grep -c . || echo "0")

    if [ "$count" -eq 0 ] || [ -z "$files" ]; then
      gum style --foreground "$NORD15" "✓ No large files found"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    gum style --foreground "$NORD13" "Found $count large files"
    echo ""

    if ! gum confirm "Review these files?"; then
      return
    fi

    while IFS= read -r file; do
      [ -z "$file" ] && continue
      review_item "$file" "Large File" || break
    done <<< "$files"
  }

  review_dev_artifacts() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Development Artifacts"

    gum spin --spinner dot --title "Scanning for artifacts..." -- sleep 1

    local existing_dirs=()
    for dir in "''${TARGET_DIRS[@]}"; do
      [ -d "$dir" ] && existing_dirs+=("$dir")
    done

    if [ ''${#existing_dirs[@]} -eq 0 ]; then
      gum style --foreground "$NORD3" "No target directories found"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    # Find common dev artifacts
    local artifacts_found=()
    for pattern in "''${ARTIFACTS[@]}"; do
      while IFS= read -r item; do
        [ -n "$item" ] && artifacts_found+=("$item")
      done < <(find "''${existing_dirs[@]}" -name "$pattern" -type d 2>/dev/null)
    done

    if [ ''${#artifacts_found[@]} -eq 0 ]; then
      gum style --foreground "$NORD15" "✓ No development artifacts found"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    gum style --foreground "$NORD13" "Found ''${#artifacts_found[@]} artifact directories"
    echo ""

    if ! gum confirm "Review these artifacts?"; then
      return
    fi

    for artifact in "''${artifacts_found[@]}"; do
      review_item "$artifact" "Development Artifact" || break
    done
  }

  review_abandoned_projects() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Abandoned Projects (> $ABANDONED_DAYS days inactive)"

    gum spin --spinner dot --title "Scanning for abandoned projects..." -- sleep 1

    local project_dirs=()
    for base in "$HOME/projects" "$HOME/development"; do
      [ -d "$base" ] && project_dirs+=("$base")
    done

    if [ ''${#project_dirs[@]} -eq 0 ]; then
      gum style --foreground "$NORD3" "No project directories found"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    local abandoned=()
    for base in "''${project_dirs[@]}"; do
      while IFS= read -r dir; do
        [ -z "$dir" ] || [ ! -d "$dir" ] && continue

        # Check if it's a git repo
        if [ -d "$dir/.git" ]; then
          local last_commit_days=$(cd "$dir" && git log -1 --format="%ct" 2>/dev/null)
          if [ -n "$last_commit_days" ]; then
            local days_ago=$(( ($(date +%s) - last_commit_days) / 86400 ))
            if [ "$days_ago" -gt "$ABANDONED_DAYS" ]; then
              abandoned+=("$dir")
            fi
          fi
        else
          # Not a git repo, check file modification time
          local last_modified=$(find "$dir" -type f -printf '%T@\n' 2>/dev/null | sort -n | tail -1 | cut -d. -f1)
          if [ -n "$last_modified" ]; then
            local days_ago=$(( ($(date +%s) - last_modified) / 86400 ))
            if [ "$days_ago" -gt "$ABANDONED_DAYS" ]; then
              abandoned+=("$dir")
            fi
          fi
        fi
      done < <(find "$base" -mindepth 1 -maxdepth 1 -type d 2>/dev/null)
    done

    if [ ''${#abandoned[@]} -eq 0 ]; then
      gum style --foreground "$NORD15" "✓ No abandoned projects found"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    gum style --foreground "$NORD13" "Found ''${#abandoned[@]} abandoned projects"
    echo ""

    if ! gum confirm "Review these projects?"; then
      return
    fi

    for project in "''${abandoned[@]}"; do
      review_item "$project" "Abandoned Project" || break
    done
  }

  review_empty_dirs() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Empty Directories"

    gum spin --spinner dot --title "Scanning for empty directories..." -- sleep 1

    local existing_dirs=()
    for dir in "''${TARGET_DIRS[@]}"; do
      [ -d "$dir" ] && existing_dirs+=("$dir")
    done

    if [ ''${#existing_dirs[@]} -eq 0 ]; then
      gum style --foreground "$NORD3" "No target directories found"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    local empty_dirs=$(find "''${existing_dirs[@]}" -type d -empty 2>/dev/null | sort)
    local count=$(echo "$empty_dirs" | grep -c . || echo "0")

    if [ "$count" -eq 0 ] || [ -z "$empty_dirs" ]; then
      gum style --foreground "$NORD15" "✓ No empty directories found"
      gum input --placeholder "Press Enter to continue..."
      return
    fi

    gum style --foreground "$NORD13" "Found $count empty directories"
    echo ""

    if ! gum confirm "Review these directories?"; then
      return
    fi

    while IFS= read -r dir; do
      [ -z "$dir" ] && continue
      review_item "$dir" "Empty Directory" || break
    done <<< "$empty_dirs"
  }

  show_summary() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Review Summary"
    echo ""

    local trash_size_human=$(numfmt --to=iec-i --suffix=B $TRASH_SIZE 2>/dev/null || echo "unknown")

    gum style --foreground "$NORD13" "Items trashed: $TRASH_COUNT"
    gum style --foreground "$NORD13" "Space freed: $trash_size_human"
    gum style --foreground "$NORD13" "Items kept: $KEEP_COUNT"
    echo ""
    gum style --foreground "$NORD3" "Session log: $SESSION_LOG"
    echo ""

    if [ "$TRASH_COUNT" -gt 0 ]; then
      gum style --foreground "$NORD14" "Tip: Use 'trash-list' to view trashed items"
      gum style --foreground "$NORD14" "Tip: Use 'trash-restore' to restore items"
      gum style --foreground "$NORD14" "Tip: Use 'trash-empty' to permanently delete"
    fi

    echo ""
    gum input --placeholder "Press Enter to exit..."
  }

  # ==========================================
  # MAIN MENU
  # ==========================================

  main_menu() {
    while true; do
      header

      local choice=$(gum choose \
        --header.foreground "$NORD14" \
        --cursor.foreground "$NORD13" \
        --header "Select category to review:" \
        "recent_downloads" \
        "large_files" \
        "dev_artifacts" \
        "abandoned_projects" \
        "empty_directories" \
        "run_all" \
        "show_summary" \
        "exit")

      case "$choice" in
        "recent_downloads")
          review_recent_downloads
          ;;
        "large_files")
          review_large_files
          ;;
        "dev_artifacts")
          review_dev_artifacts
          ;;
        "abandoned_projects")
          review_abandoned_projects
          ;;
        "empty_directories")
          review_empty_dirs
          ;;
        "run_all")
          review_recent_downloads
          review_large_files
          review_dev_artifacts
          review_abandoned_projects
          review_empty_dirs
          show_summary
          return
          ;;
        "show_summary")
          show_summary
          ;;
        "exit")
          show_summary
          return
          ;;
      esac
    done
  }

  # ==========================================
  # ENTRY POINT
  # ==========================================

  log_action "=== File Review Session Started ==="
  main_menu
  log_action "=== File Review Session Ended ==="
''
