{pkgs}:
pkgs.writeShellScriptBin "rofi-daily-capture" ''
  set -euo pipefail

  ROFI="${pkgs.rofi}/bin/rofi"
  DATE="${pkgs.coreutils}/bin/date"
  MKDIR="${pkgs.coreutils}/bin/mkdir"
  MKTEMP="${pkgs.coreutils}/bin/mktemp"
  MV="${pkgs.coreutils}/bin/mv"
  PRINTF="${pkgs.coreutils}/bin/printf"
  AWK="${pkgs.gawk}/bin/awk"

  DAILY_DIR="$HOME/documents/projects/org-roam/daily"

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

    "$PRINTF" "%s\n" \
      "#+title: $date_title" \
      "#+filetags: :daily:" \
      "" \
      "* morning" \
      "** priorities" \
      "" \
      "* session log" \
      "" \
      "* habits" \
      "** TODO exercise :habit:" \
      "   SCHEDULED: <$date_scheduled +1d>" \
      "** TODO review inbox :habit:" \
      "   SCHEDULED: <$date_scheduled +1d>" \
      "" \
      "* metrics" \
      ":PROPERTIES:" \
      ":STEPS:" \
      ":PAGES:" \
      ":EXERCISE_MIN:" \
      ":END:" \
      "" \
      "* scratch" \
      "" \
      "* shutdown" \
      "- [ ] session end protocol completed" \
      "- [ ] inbox processed" \
      >"$file"
  }

  insert_under_scratch() {
    local file="$1"
    local entry="$2"
    local tmp
    tmp="$("$MKTEMP")"

    "$AWK" -v entry="$entry" '
      BEGIN { inserted = 0 }
      {
        print
        if (!inserted && $0 ~ /^\\* scratch\\b/) {
          print entry
          inserted = 1
        }
      }
      END {
        if (!inserted) {
          print ""
          print "* scratch"
          print entry
        }
      }
    ' "$file" >"$tmp"

    "$MV" "$tmp" "$file"
  }

  prompt_note() {
    "$PRINTF" "%s\n" "" | "$ROFI" -dmenu -p "scratch" -theme-str 'listview { enabled: false; } textbox-prompt-colon { enabled: false; }' || true
  }

  main() {
    local today
    local file
    today="$("$DATE" "+%Y-%m-%d")"
    file="$DAILY_DIR/$today.org"

    local note
    note="$(prompt_note)"
    if [ -z "''${note:-}" ]; then
      exit 0
    fi

    ensure_daily_file "$file"

    local ts
    ts="$("$DATE" "+%H:%M:%S")"
    insert_under_scratch "$file" "$ts - $note"
  }

  main
''
