{pkgs}:
pkgs.writeShellScriptBin "rofi-daily-capture" ''
  set -euo pipefail

  ROFI="${pkgs.rofi}/bin/rofi"
  DATE="${pkgs.coreutils}/bin/date"
  MKDIR="${pkgs.coreutils}/bin/mkdir"
  PRINTF="${pkgs.coreutils}/bin/printf"
  AWK="${pkgs.gawk}/bin/awk"

  DAILY_DIR="$HOME/documents/projects/org-roam/daily"
  TEMPLATE_FILE="$HOME/.emacs.d/templates/daily.org.tmpl"
  ROFI_THEME_STR='listview { enabled: false; } textbox-prompt-colon { enabled: false; } window { border: 2px; border-color: #ffffff; border-radius: 12px; width: 25%; } mainbox { padding: 12px; } entry { padding: 6px; }'

  ensure_daily_file() {
    local file="$1"

    "$MKDIR" -p "$DAILY_DIR"

    if [ -f "$file" ]; then
      return 0
    fi

    local date_title
    local date_scheduled
    date_title="$("$DATE" "+%Y-%m-%d %A")"
    date_scheduled="$("$DATE" "+%Y-%m-%d %a")"

    if [ -f "$TEMPLATE_FILE" ]; then
      "$AWK" -v title="$date_title" -v scheduled="$date_scheduled" '
        {
          gsub(/\\{\\{DATE_TITLE\\}\\}/, title)
          gsub(/\\{\\{DATE_SCHEDULED\\}\\}/, scheduled)
          print
        }
      ' "$TEMPLATE_FILE" >"$file"
    else
      "$PRINTF" "%s\n" \
        "#+title: $date_title" \
        "#+filetags: :daily:" \
        "" \
        "* scratch" \
        "" \
        >"$file"
    fi
  }

  trim() {
    local s="$1"
    s="''${s#"''${s%%[![:space:]]*}"}"
    s="''${s%"''${s##*[![:space:]]}"}"
    "$PRINTF" "%s" "$s"
  }

  prompt_note() {
    "$PRINTF" "%s\n" "" | "$ROFI" -dmenu -p "scratch: " -theme-str "$ROFI_THEME_STR" || true
  }

  main() {
    local today
    local file
    today="$("$DATE" "+%Y-%m-%d")"
    file="$DAILY_DIR/$today.org"

    local note
    note="$(trim "$(prompt_note)")"
    if [ -z "''${note}" ]; then
      exit 0
    fi

    ensure_daily_file "$file"

    local ts
    ts="$("$DATE" "+%H:%M:%S")"
    "$PRINTF" "%s\n" "$ts - $note" >>"$file"
  }

  main
''
