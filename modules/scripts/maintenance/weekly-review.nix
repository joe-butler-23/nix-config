{pkgs}:
pkgs.writeShellScriptBin "weekly-review" ''
  export PATH="${pkgs.gum}/bin:${pkgs.coreutils}/bin:${pkgs.gnused}/bin:$PATH"

  NORD11="#BF616A"
  NORD13="#EBCB8B"
  NORD14="#A3BE8C"
  NORD15="#B48EAD"
  NORD6="#ECEFF4"
  NORD3="#4C566A"

  gum_choose() {
    command gum choose "$@" </dev/tty
  }

  gum_input() {
    command gum input "$@" </dev/tty
  }

  strip_ansi() {
    sed -r 's/\x1B\[[0-9;]*[mK]//g'
  }

  header() {
    clear
    gum style --foreground "$NORD6" --border double --border-foreground "$NORD13" --padding "1 2" --margin "1 0" --align center "Weekly Review"
  }

  run_step() {
    local id="$1"
    case "$id" in
      "file_review") file-review ;;
      "system_maintenance") system-maintenance ;;
      "metrics") weekly-metrics ;;
      *) return 1 ;;
    esac
  }

  declare -a STEP_ORDER=(
    "file_review"
    "system_maintenance"
    "metrics"
  )

  declare -A STEP_LABEL=(
    ["file_review"]="File review"
    ["system_maintenance"]="System maintenance"
    ["metrics"]="Weekly metrics"
  )

  declare -A done=()

  while true; do
    header
    gum style --foreground "$NORD3" "Esc backs out of sub-menus; add steps via STEP_ORDER/STEP_LABEL."
    echo ""

    declare -a options=()
    declare -A label_to_id=()

    for id in "''${STEP_ORDER[@]}"; do
      label="''${STEP_LABEL[$id]}"
      label_to_id["$label"]="$id"

      if [ "''${done[$id]:-0}" -eq 1 ]; then
        options+=("$(printf "\033[32m[✓]\033[0m %s" "$label")")
      else
        options+=("$(printf "[ ] %s" "$label")")
      fi
    done

    options+=("run_remaining")
    options+=("reset")
    options+=("exit")

    choice=""
    if ! choice="$(gum_choose \
      --header.foreground "$NORD14" \
      --cursor.foreground "$NORD13" \
      --selected.foreground "$NORD13" \
      "''${options[@]}")"; then
      exit 0
    fi

    clean_choice="$(printf '%s' "$choice" | strip_ansi)"

    case "$clean_choice" in
      "run_remaining")
        for id in "''${STEP_ORDER[@]}"; do
          if [ "''${done[$id]:-0}" -eq 1 ]; then
            continue
          fi
          run_step "$id" || true
          done["$id"]=1
        done
        ;;
      "reset")
        done=()
        ;;
      "exit")
        exit 0
        ;;
      "[ ] "*|"[✓] "*)
        label="''${clean_choice#"[ ] "}"
        label="''${label#"[✓] "}"
        id="''${label_to_id[$label]}"
        [ -z "$id" ] && continue
        run_step "$id" || true
        done["$id"]=1
        ;;
    esac
  done
''
