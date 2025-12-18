{pkgs}:
pkgs.writeShellScriptBin "weekly-metrics" ''
  export PATH="${pkgs.gum}/bin:${pkgs.coreutils}/bin:${pkgs.gnused}/bin:${pkgs.xdg-utils}/bin:$PATH"

  NORD11="#BF616A"
  NORD13="#EBCB8B"
  NORD14="#A3BE8C"
  NORD15="#B48EAD"
  NORD6="#ECEFF4"
  NORD3="#4C566A"

  gum_input() {
    command gum input "$@" </dev/tty
  }

  gum_confirm() {
    command gum confirm "$@" </dev/tty
  }

  header() {
    clear
    gum style --foreground "$NORD6" --border double --border-foreground "$NORD13" --padding "1 2" --margin "1 0" --align center "Weekly Metrics"
  }

  METRICS_DIR="''${XDG_DATA_HOME:-$HOME/.local/share}/weekly-review"
  METRICS_FILE="$METRICS_DIR/metrics.tsv"
  mkdir -p "$METRICS_DIR"

  header
  gum style --foreground "$NORD3" "Saving to: $METRICS_FILE"
  echo ""

  date_default="$(date +%Y-%m-%d)"
  week_date="$(gum_input --value "$date_default" --placeholder "Week ending date (YYYY-MM-DD)")" || exit 0

  rmssd="$(gum_input --placeholder "RMSSD (optional)")" || exit 0
  sdnn="$(gum_input --placeholder "SDNN (optional)")" || exit 0
  lfhf="$(gum_input --placeholder "LF/HF ratio (optional)")" || exit 0
  steps="$(gum_input --placeholder "Weekly steps total (optional)")" || exit 0
  notes="$(gum_input --placeholder "Notes (optional)")" || exit 0

  if [ ! -f "$METRICS_FILE" ]; then
    printf "week_date\trmssd\tsdnn\tlfhf\tsteps\tnotes\n" > "$METRICS_FILE"
  fi

  printf "%s\t%s\t%s\t%s\t%s\t%s\n" \
    "$week_date" \
    "$rmssd" \
    "$sdnn" \
    "$lfhf" \
    "$steps" \
    "$notes" >> "$METRICS_FILE"

  echo ""
  gum style --foreground "$NORD15" "Saved."
  if gum_confirm "Open metrics file?"; then
    if command -v xdg-open >/dev/null 2>&1; then
      nohup xdg-open "$METRICS_FILE" >/dev/null 2>&1 &
    fi
  fi
''
