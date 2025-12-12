#!/usr/bin/env bash
# Open today's org-roam daily note for quick scratch capture
# Mimics refile.org behavior by opening file directly

# Compute today's date
DAILY_NAME=$(date +%Y-%m-%d).org
DAILY_PATH="$HOME/documents/projects/org-roam/daily/$DAILY_NAME"

# Ensure directory exists
mkdir -p "$(dirname "$DAILY_PATH")"

# Create file if it doesn't exist (with template)
if [ ! -f "$DAILY_PATH" ]; then
  DATE_TITLE=$(date "+%Y-%m-%d %A")
  DATE_SCHEDULED=$(date "+%Y-%m-%d %a")

  cat > "$DAILY_PATH" <<EOF
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
exec emacsclient -c -F "((name . \"daily-scratch\"))" -a "" "$DAILY_PATH"
