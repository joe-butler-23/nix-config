{pkgs}:
pkgs.writeShellScriptBin "study-focus" ''
  set -euo pipefail

  STATE_FILE="/tmp/study-focus-state"
  BLOCKED_APPS="firefox google-chrome brave"

  block_loop() {
      while [[ -f "$STATE_FILE" ]]; do
          for app in $BLOCKED_APPS; do
              ${pkgs.procps}/bin/pkill -f "$app" 2>/dev/null || true
          done
          sleep 2
      done
  }

  case "''${1:-toggle}" in
      on)
          [[ -f "$STATE_FILE" ]] && exit 0
          echo "active" > "$STATE_FILE"
          ${pkgs.libnotify}/bin/notify-send "Study Focus" "Study mode ON"
          block_loop &
          ;;
      off)
          rm -f "$STATE_FILE"
          ${pkgs.libnotify}/bin/notify-send "Study Focus" "Study mode OFF"
          ;;
      toggle)
          [[ -f "$STATE_FILE" ]] && "$0" off || "$0" on
          ;;
  esac
''
