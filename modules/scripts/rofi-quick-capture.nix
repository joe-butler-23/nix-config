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
    name = "rofi-quick-capture";
    runtimeInputs = with pkgs; [coreutils rofi gnused];

    text = ''
      set -euo pipefail

      REFILE_FILE="$HOME/projects/refile.org"

      # Interpolate the theme string. Quote it for the shell.
      ROFI_THEME_STR="${rofiTheme}"

      ensure_refile_file() {
        local file="$1"
        mkdir -p "$(dirname "$file")"

        [ -f "$file" ] && return 0

        printf "%s\n" \
          "#+title: Refile" \
          "#+filetags: :refile:" \
          "" \
          >"$file"
      }

      append_refile_entry() {
        local file="$1"
        local note="$2"

        local ts
        ts="$(date "+%Y-%m-%d %a %H:%M")"

        printf "%s\n" \
          "* scratch $note" \
          "  Captured: [$ts]" \
          "" \
          >>"$file"
      }

      main() {
        local note
        # echo "" | ... prevents rofi from waiting on stdin
        note=$(echo "" | rofi -dmenu -p "scratch: " -theme-str "$ROFI_THEME_STR" || true)

        # Trim whitespace using sed
        note=$(echo "$note" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')

        if [ -z "$note" ]; then
          exit 0
        fi

        ensure_refile_file "$REFILE_FILE"
        append_refile_entry "$REFILE_FILE" "$note"
      }

      main
    '';
  }
