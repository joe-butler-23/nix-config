#!/usr/bin/env bash
# Log Claude's skill usage decisions

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Load logging utilities
source "$SCRIPT_DIR/logger.sh"

skills_used="$1"
reason="$2"

if [[ -z "$skills_used" ]] || [[ -z "$reason" ]]; then
    echo "Usage: $0 '<skills>' '<reason>'"
    echo "Examples:"
    echo "  $0 'nix-development-guidelines' 'User has command not found error'"
    echo "  $0 'beads-workflow,session-handoff' 'Task management and session ending'"
    echo "  $0 'none' 'Simple question, no specialized skills needed'"
    exit 1
fi

if [[ "$skills_used" == "none" ]]; then
    log_message "skill-decision" "NO SKILLS USED - Reason: $reason"
else
    log_message "skill-decision" "SKILLS ACTIVATED: $skills_used - Reason: $reason"
fi
