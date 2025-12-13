{pkgs}: let
  rofiTheme = ''
    listview { enabled: false; }
    textbox-prompt-colon { enabled: false; }

    window {
      border: 2px;
      border-color: #000000;
      border-radius: 12px;
      width: 25%;
      padding: 6px;
    }

    mainbox { padding: 6px; }
    inputbar { padding: 6px; }
    prompt { padding: 0px; }
    entry  { padding: 0px; }
  '';
in
  pkgs.writeShellApplication {
    name = "rofi-daily-capture";
    runtimeInputs = with pkgs; [coreutils rofi gnused];

    text = ''
      DAILY_DIR="$HOME/documents/projects/org-roam/daily"
      TEMPLATE_FILE="$HOME/documents/projects/org-roam/templates/daily.org.tmpl"

      # Interpolate the theme string. Quote it for the shell.
      ROFI_THEME_STR="${rofiTheme}"

      ensure_daily_file() {
        local file="$1"

        mkdir -p "$DAILY_DIR"

        if [ -f "$file" ]; then
          return 0
        fi

        local date_title
        local date_scheduled
        date_title="$(date "+%Y-%m-%d %A")"
        date_scheduled="$(date "+%Y-%m-%d %a")"

        if [ -f "$TEMPLATE_FILE" ]; then
          sed -e "s/{{DATE_TITLE}}/$date_title/" \
              -e "s/{{DATE_SCHEDULED}}/$date_scheduled/" \
              "$TEMPLATE_FILE" > "$file"
        else
          printf "%s\n" \
            "#+title: $date_title" \
            "#+filetags: :daily:" \
            "" \
            "* scratch" \
            "" \
            >"$file"
        fi
      }

      main() {
        local today
        local file
        today="$(date "+%Y-%m-%d")"
        file="$DAILY_DIR/$today.org"

        # Capture note
        local note
        # echo "" | ... prevents rofi from waiting on stdin
        note=$(echo "" | rofi -dmenu -p "scratch: " -theme-str "$ROFI_THEME_STR" || true)

        # Trim whitespace using sed
        note=$(echo "$note" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')

        if [ -z "$note" ]; then
          exit 0
        fi

        ensure_daily_file "$file"

        local ts
        ts="$(date "+%H:%M:%S")"
        echo "$ts - $note" >> "$file"
      }

      main
    '';
  }
