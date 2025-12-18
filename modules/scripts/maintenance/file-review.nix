{pkgs}:
pkgs.writeShellScriptBin "file-review" ''
  export PATH="${pkgs.gum}/bin:${pkgs.fd}/bin:${pkgs.coreutils}/bin:${pkgs.trash-cli}/bin:${pkgs.gnugrep}/bin:${pkgs.findutils}/bin:${pkgs.git}/bin:${pkgs.gnused}/bin:${pkgs.file}/bin:$PATH"

  # ==========================================
  # CONFIGURATION
  # ==========================================

  # Thresholds
  LARGE_FILE_SIZE="100M"
  RECENT_DAYS=7

  # Target directories for scanning
  TARGET_DIRS=(
    "$HOME/downloads"
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

  gum_choose() {
    command gum choose "$@" </dev/tty
  }

  gum_confirm() {
    command gum confirm "$@" </dev/tty
  }

  gum_input() {
    command gum input "$@" </dev/tty
  }

  open_directory() {
    local dir="$1"

    if command -v thunar >/dev/null 2>&1; then
      nohup thunar "$dir" >/dev/null 2>&1 &
      return 0
    fi

    if command -v xdg-open >/dev/null 2>&1; then
      nohup xdg-open "$dir" >/dev/null 2>&1 &
      return 0
    fi

    return 1
  }

  format_bytes() {
    local bytes="$1"
    numfmt --to=iec-i --suffix=B "$bytes" 2>/dev/null || echo "''${bytes}B"
  }

  resolve_dir() {
    for candidate in "$@"; do
      if [ -d "$candidate" ]; then
        echo "$candidate"
        return 0
      fi
    done
    return 1
  }

  get_review_dirs() {
    local -a review_dirs=()

    local dir
    dir="$(resolve_dir "$HOME/downloads" "$HOME/Downloads")" && review_dirs+=("$dir")
    dir="$(resolve_dir "$HOME/development" "$HOME/Development")" && review_dirs+=("$dir")
    dir="$(resolve_dir "$HOME/documents" "$HOME/Documents")" && review_dirs+=("$dir")
    dir="$(resolve_dir "$HOME/projects" "$HOME/Projects")" && review_dirs+=("$dir")
    dir="$(resolve_dir "$HOME/utilities" "$HOME/Utilities")" && review_dirs+=("$dir")

    printf '%s\n' "''${review_dirs[@]}"
  }

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
    local action=$(gum_choose --header.foreground "$NORD14" \
      "keep" \
      "trash" \
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
        ;;
      "keep")
        gum style --foreground "$NORD3" "Kept"
        log_action "KEEP: $item"
        KEEP_COUNT=$((KEEP_COUNT + 1))
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
        gum_input --placeholder "Press Enter to continue..."
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

  list_recent_directories() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Recent Directories (< $RECENT_DAYS days)"

    local -a review_dirs=()
    mapfile -t review_dirs < <(get_review_dirs)

    if [ ''${#review_dirs[@]} -eq 0 ]; then
      gum style --foreground "$NORD3" "No review directories found"
      gum_input --placeholder "Press Enter to continue..."
      return
    fi

    gum spin --spinner dot --title "Scanning recent directories..." -- sleep 0.2

    local -A count_by_bucket=()
    local -A last_ts_by_bucket=()
    local -A has_child_bucket=()

    for root in "''${review_dirs[@]}"; do
      root="''${root%/}"

      while IFS=$'\t' read -r ts path; do
        [ -z "$path" ] && continue

        local ts_int="''${ts%.*}"
        local rel="''${path#"$root"/}"
        local bucket

        if [ "$rel" = "$path" ]; then
          bucket="$root"
        else
          local first="''${rel%%/*}"
          if [ "$first" = "$rel" ]; then
            bucket="$root"
          else
            bucket="$root/$first"
            has_child_bucket["$root"]=1
          fi
        fi

        count_by_bucket["$bucket"]=$(( ''${count_by_bucket["$bucket"]:-0} + 1 ))

        local prev="''${last_ts_by_bucket["$bucket"]:-0}"
        if [ "$ts_int" -gt "$prev" ]; then
          last_ts_by_bucket["$bucket"]="$ts_int"
        fi
      done < <(
        find "$root" \
          \( \
            -path '*/.git/*' -o \
            -path '*/node_modules/*' -o \
            -path '*/.venv/*' -o \
            -path '*/venv/*' -o \
            -path '*/__pycache__/*' -o \
            -path '*/.pytest_cache/*' -o \
            -path '*/target/*' -o \
            -path '*/dist/*' -o \
            -path '*/build/*' -o \
            -path '*/.cache/*' \
          \) -prune -o \
          -type f \
          -mtime -"$RECENT_DAYS" \
          ! -name '*.pyc' \
          ! -name '.DS_Store' \
          ! -name 'Thumbs.db' \
          -printf '%T@\t%p\n' 2>/dev/null
      )
    done

    if [ ''${#count_by_bucket[@]} -eq 0 ]; then
      gum style --foreground "$NORD15" "✓ No recent directories to review"
      gum_input --placeholder "Press Enter to continue..."
      return
    fi

    local tmp
    tmp="$(mktemp)"
    trap 'rm -f "$tmp"' RETURN

    for bucket in "''${!count_by_bucket[@]}"; do
      for root in "''${review_dirs[@]}"; do
        root="''${root%/}"
        if [ "$bucket" = "$root" ] && [ -n "''${has_child_bucket[$root]+x}" ]; then
          continue 2
        fi
      done

      local ts="''${last_ts_by_bucket["$bucket"]:-0}"
      local count="''${count_by_bucket["$bucket"]:-0}"
      local last_str
      last_str="$(date -d "@$ts" '+%Y-%m-%d %H:%M' 2>/dev/null || echo "unknown")"
      printf "%s\t%04d\t%s\t%s\n" "$ts" "$count" "$last_str" "$bucket" >> "$tmp"
    done

    sort -nr "$tmp" -o "$tmp"

    local -A reviewed_buckets=()

    while true; do
      header
      gum style --foreground "$NORD13" "Directories with recently modified files"
      echo ""

      strip_ansi() {
        sed -r 's/\x1B\[[0-9;]*[mK]//g'
      }

      bucket_from_display() {
        local display="$1"
        display="''${display%/}"

        if [[ "$display" == "~/"* ]]; then
          echo "$HOME/''${display#~/}"
          return 0
        fi

        if [[ "$display" == "~" ]]; then
          echo "$HOME"
          return 0
        fi

        echo "$display"
      }

      local -a options=()

      while IFS=$'\t' read -r _ts count last_str bucket; do
        [ -z "$bucket" ] && continue

        local display_bucket
        display_bucket="''${bucket/#$HOME/~}"
        display_bucket="''${display_bucket%/}/"

        local line
        line="$(printf "%s\t%s\t%s" "$count" "$last_str" "$display_bucket")"
        if [ -n "''${reviewed_buckets[$bucket]+x}" ]; then
          options+=("$(printf "\033[90m%s\033[0m" "$line")")
        else
          options+=("$line")
        fi
      done < "$tmp"

      local selection
      selection="$(gum_choose --height 20 --header "Enter opens in Thunar (Esc to go back)" "''${options[@]}")"
      if [ -z "$selection" ]; then
        return
      fi

      local clean_selection
      clean_selection="$(printf '%s' "$selection" | strip_ansi)"

      local display_bucket
      display_bucket="$(printf '%s' "$clean_selection" | cut -f3-)"

      local selected_bucket
      selected_bucket="$(bucket_from_display "$display_bucket")"
      [ -z "$selected_bucket" ] && continue

      if open_directory "$selected_bucket"; then
        reviewed_buckets["$selected_bucket"]=1
      else
        gum style --foreground "$NORD11" "Could not open directory (need thunar or xdg-open)"
        gum_input --placeholder "Press Enter to continue..."
      fi
    done
  }

  review_recent_files() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Recent Files (< $RECENT_DAYS days)"

    local -a review_dirs=()
    mapfile -t review_dirs < <(get_review_dirs)

    if [ ''${#review_dirs[@]} -eq 0 ]; then
      gum style --foreground "$NORD3" "No review directories found"
      gum_input --placeholder "Press Enter to continue..."
      return
    fi

    gum spin --spinner dot --title "Scanning recent files..." -- sleep 0.2

    local tmp
    tmp="$(mktemp)"
    trap 'rm -f "$tmp"' RETURN

    for dir in "''${review_dirs[@]}"; do
      find "$dir" \
        \( \
          -path '*/.git/*' -o \
          -path '*/node_modules/*' -o \
          -path '*/.venv/*' -o \
          -path '*/venv/*' -o \
          -path '*/__pycache__/*' -o \
          -path '*/.pytest_cache/*' -o \
          -path '*/target/*' -o \
          -path '*/dist/*' -o \
          -path '*/build/*' -o \
          -path '*/.cache/*' \
        \) -prune -o \
        -type f \
        -mtime -"$RECENT_DAYS" \
        ! -name '*.pyc' \
        ! -name '.DS_Store' \
        ! -name 'Thumbs.db' \
        -printf '%T@\t%s\t%p\n' 2>/dev/null
    done | sort -nr > "$tmp"

    local count
    count="$(wc -l < "$tmp" | tr -d ' ')"

    if [ "$count" -eq 0 ]; then
      gum style --foreground "$NORD15" "✓ No recent files to review"
      gum_input --placeholder "Press Enter to continue..."
      return
    fi

    local remaining
    remaining="$(mktemp)"
    trap 'rm -f "$tmp" "$remaining"' RETURN
    cp "$tmp" "$remaining"

    while true; do
      local lines_total
      lines_total="$(wc -l < "$remaining" | tr -d ' ')"

      if [ "$lines_total" -eq 0 ]; then
        gum style --foreground "$NORD15" "✓ No remaining recent files to review"
        gum_input --placeholder "Press Enter to continue..."
        return
      fi

      local -a display_lines=()
      local -A bytes_by_path=()
      local idx=0

      while IFS=$'\t' read -r _ts bytes path; do
        [ -z "$path" ] && continue
        idx=$((idx + 1))
        bytes_by_path["$path"]="$bytes"
        display_lines+=("$(printf "%04d\t%s\t%s" "$idx" "$(format_bytes "$bytes")" "$path")")
      done < "$remaining"

      header
      gum style --foreground "$NORD13" "Found $lines_total recent files (select one or more)"
      echo ""

      local selection
      selection="$(gum_choose --no-limit --height 20 --header "Space to select, Enter to confirm" "''${display_lines[@]}")"
      if [ -z "$selection" ]; then
        return
      fi

      local action
      action="$(gum_choose --header.foreground "$NORD14" \
        "trash_selected" \
        "mark_kept" \
        "preview_first" \
        "done")"

      case "$action" in
        "trash_selected")
          if ! gum_confirm "Trash selected files?"; then
            continue
          fi

          local -A selected_paths=()
          while IFS=$'\t' read -r _i _size path; do
            [ -z "$path" ] && continue
            selected_paths["$path"]=1
          done <<< "$selection"

          for path in "''${!selected_paths[@]}"; do
            [ -e "$path" ] || continue
            if trash-put "$path" 2>/dev/null; then
              local bytes="''${bytes_by_path[$path]:-0}"
              TRASH_COUNT=$((TRASH_COUNT + 1))
              TRASH_SIZE=$((TRASH_SIZE + bytes))
              log_action "TRASH: $path ($(format_bytes "$bytes"))"
            else
              log_action "ERROR: Failed to trash $path"
            fi
          done

          local new_remaining
          new_remaining="$(mktemp)"
          while IFS=$'\t' read -r ts bytes path; do
            if [ -z "$path" ] || [ -z "''${selected_paths[$path]+x}" ]; then
              printf "%s\t%s\t%s\n" "$ts" "$bytes" "$path" >> "$new_remaining"
            fi
          done < "$remaining"
          mv "$new_remaining" "$remaining"
          ;;

        "mark_kept")
          local -A kept_paths=()
          while IFS=$'\t' read -r _i _size path; do
            [ -z "$path" ] && continue
            kept_paths["$path"]=1
            KEEP_COUNT=$((KEEP_COUNT + 1))
            log_action "KEEP: $path"
          done <<< "$selection"

          local new_remaining
          new_remaining="$(mktemp)"
          while IFS=$'\t' read -r ts bytes path; do
            if [ -z "$path" ] || [ -z "''${kept_paths[$path]+x}" ]; then
              printf "%s\t%s\t%s\n" "$ts" "$bytes" "$path" >> "$new_remaining"
            fi
          done < "$remaining"
          mv "$new_remaining" "$remaining"
          ;;

        "preview_first")
          local first_path
          first_path="$(printf "%s\n" "$selection" | head -n 1 | cut -f3-)"
          [ -n "$first_path" ] && review_item "$first_path" "Recent File (preview)"
          ;;

        "done")
          return
          ;;
      esac
    done
  }

  review_large_files() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Large Files (> 100MB)"

    gum spin --spinner dot --title "Scanning for large files..." -- sleep 0.2

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
      gum_input --placeholder "Press Enter to continue..."
      return
    fi

    gum style --foreground "$NORD13" "Found $count large files"
    echo ""

    if ! gum_confirm "Review these files?"; then
      return
    fi

    local idx=0
    while IFS= read -r file; do
      [ -z "$file" ] && continue
      idx=$((idx + 1))
      review_item "$file" "Large File ($idx/$count)" || break
    done <<< "$files"
  }

  review_dev_artifacts() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Development Artifacts"

    gum spin --spinner dot --title "Scanning for artifacts..." -- sleep 0.2

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
      done < <(find "''${existing_dirs[@]}" -name "$pattern" \( -type d -o -type f \) 2>/dev/null)
    done

    if [ ''${#artifacts_found[@]} -eq 0 ]; then
      gum style --foreground "$NORD15" "✓ No development artifacts found"
      gum_input --placeholder "Press Enter to continue..."
      return
    fi

    gum style --foreground "$NORD13" "Found ''${#artifacts_found[@]} artifacts"
    echo ""

    if ! gum_confirm "Review these artifacts?"; then
      return
    fi

    for artifact in "''${artifacts_found[@]}"; do
      review_item "$artifact" "Development Artifact" || break
    done
  }

  review_empty_dirs() {
    header
    gum style --foreground "$NORD14" --bold --margin "1 0" "Category: Empty Directories"

    gum spin --spinner dot --title "Scanning for empty directories..." -- sleep 0.2

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
      gum_input --placeholder "Press Enter to continue..."
      return
    fi

    gum style --foreground "$NORD13" "Found $count empty directories"
    echo ""

    if ! gum_confirm "Review these directories?"; then
      return
    fi

    local idx=0
    while IFS= read -r dir; do
      [ -z "$dir" ] && continue
      idx=$((idx + 1))
      review_item "$dir" "Empty Directory ($idx/$count)" || break
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
    gum_input --placeholder "Press Enter to exit..."
  }

  # ==========================================
  # MAIN MENU
  # ==========================================

  main_menu() {
    while true; do
      header

      local choice=$(gum_choose \
        --header.foreground "$NORD14" \
        --cursor.foreground "$NORD13" \
        --header "Select category to review:" \
        "recent_directories" \
        "recent_files" \
        "large_files" \
        "dev_artifacts" \
        "empty_directories" \
        "show_summary" \
        "exit")

      case "$choice" in
        "recent_directories")
          list_recent_directories
          ;;
        "recent_files")
          review_recent_files
          ;;
        "large_files")
          review_large_files
          ;;
        "dev_artifacts")
          review_dev_artifacts
          ;;
        "empty_directories")
          review_empty_dirs
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
