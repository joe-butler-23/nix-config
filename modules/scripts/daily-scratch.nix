{pkgs}:
pkgs.writeShellScriptBin "daily-scratch" ''
    set -euo pipefail

    # Compute today's date
    DAILY_NAME=$(${pkgs.coreutils}/bin/date +%Y-%m-%d).org
    DAILY_PATH="$HOME/documents/projects/org-roam/daily/$DAILY_NAME"

    # Ensure directory exists
    ${pkgs.coreutils}/bin/mkdir -p "$(${pkgs.coreutils}/bin/dirname "$DAILY_PATH")"

    # Create file if it doesn't exist (with template)
    if [ ! -f "$DAILY_PATH" ]; then
      DATE_TITLE=$(${pkgs.coreutils}/bin/date "+%Y-%m-%d %A")
      DATE_SCHEDULED=$(${pkgs.coreutils}/bin/date "+%Y-%m-%d %a")

      ${pkgs.coreutils}/bin/cat > "$DAILY_PATH" <<EOF
  #+title: $DATE_TITLE
  #+filetags: :daily:

  * morning
  ** priorities

  * session log

  * habits
  ** TODO exercise :habit:
     SCHEDULED: <$DATE_SCHEDULED +1d>
  ** TODO review inbox :habit:
     SCHEDULED: <$DATE_SCHEDULED +1d>

  * metrics
  :PROPERTIES:
  :STEPS:
  :PAGES:
  :EXERCISE_MIN:
  :END:

  * scratch

  * shutdown
  - [ ] session end protocol completed
  - [ ] inbox processed
  EOF
    fi

    # Open with emacsclient (directly like refile.org)
    # Assuming emacsclient is in the user's path (part of emacs package)
    exec emacsclient -c -F "((name . \"daily-scratch\"))" -a "" "$DAILY_PATH"
''
