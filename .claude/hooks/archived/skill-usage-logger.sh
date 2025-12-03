#!/usr/bin/env bash
# Skill usage logger - tracks when Claude decides to use specific skills

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Load logging utilities
source "$SCRIPT_DIR/logger.sh"

# Function to log skill usage
log_skill_usage() {
    local skill_name="$1"
    local reason="$2"
    local user_prompt_preview="${3:-}"

    if [[ -z "$skill_name" ]]; then
        echo "Usage: log_skill_usage <skill_name> <reason> [prompt_preview]"
        exit 1
    fi

    log_message "skill-usage" "ACTIVATED: $skill_name - Reason: $reason"

    if [[ -n "$user_prompt_preview" ]]; then
        log_message "skill-usage" "CONTEXT: $skill_name - Prompt: ${user_prompt_preview:0:100}..."
    fi
}

# Function to log skill consideration (when Claude explicitly considers but doesn't use)
log_skill_consideration() {
    local skill_name="$1"
    local reason="$2"

    log_message "skill-usage" "CONSIDERED: $skill_name - Decision: $reason"
}

# If called directly, log the skill usage
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "$1" in
        "use"|"activate")
            log_skill_usage "$2" "$3" "$4"
            ;;
        "consider")
            log_skill_consideration "$2" "$3"
            ;;
        *)
            echo "Usage: $0 {use|consider} <skill_name> <reason> [prompt_preview]"
            echo ""
            echo "Examples:"
            echo "  $0 use nix-development-guidelines 'User has command not found error'"
            echo "  $0 consider beads-workflow 'No .beads directory found, not applicable'"
            exit 1
            ;;
    esac
fi
