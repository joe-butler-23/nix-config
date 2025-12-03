#!/usr/bin/env bash
set -e

# Skill loader hook - displays available skills XML for AI context
# This runs on every UserPromptSubmit to show available skills

# Logging configuration
LOG_DIR="$HOME/.claude/logs"
LOG_FILE="$LOG_DIR/hooks.log"
HOOK_NAME="skill-loader"

# Create log directory if it doesn't exist
mkdir -p "$LOG_DIR"

# Logging functions
log_message() {
    local message="$1"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')

    # Check if log rotation is needed (limit to 1000 lines)
    if [[ -f "$LOG_FILE" ]]; then
        local line_count=$(wc -l < "$LOG_FILE")
        if [[ $line_count -ge 1000 ]]; then
            mv "$LOG_FILE" "${LOG_FILE}.old"
        fi
    fi

    echo "[$timestamp] [$HOOK_NAME] $message" >> "$LOG_FILE"
    echo "[$timestamp] [$HOOK_NAME] $message" >&2
}

log_hook_start() {
    local input_preview="${1:0:100}"
    log_message "STARTED - Input preview: $input_preview"
}

log_hook_end() {
    local status="$1"
    log_message "FINISHED - Status: $status"
}

log_info() {
    local info="$1"
    log_message "INFO: $info"
}

# Function to display available skills
display_skills() {
    local xml_file="$CLAUDE_PROJECT_DIR/.claude/skills/available_skills.xml"

    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "🎯 AVAILABLE SKILLS"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""

    if [[ -f "$xml_file" ]]; then
        log_info "Loading skills XML: $xml_file"
        cat "$xml_file"
        echo ""
        echo "💡 Claude: You MUST review the above skills. Where you identify any relevant to the user's request, you MUST activate them. ACTION: Where you identify a relevant skill you must use Skill tool BEFORE responding. And you must then let the user know which skills have been used."
    else
        log_info "No skills XML found at: $xml_file"
        echo "⚠️  No skills configuration found"
    fi
}

# Read input from stdin
input=$(cat)

# Log hook start
log_hook_start "$input"

# Display available skills
display_skills

log_hook_end "SUCCESS"
