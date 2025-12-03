#!/usr/bin/env bash
set -e

# Skill activation prompt hook - analyzes prompts and suggests relevant skills
# This runs on UserPromptSubmit to intelligently suggest skills based on content

# Logging configuration
LOG_DIR="$HOME/.claude/logs"
LOG_FILE="$LOG_DIR/hooks.log"
HOOK_NAME="skill-activation-prompt"

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

# Read input from stdin
input=$(cat)

# Log hook start
log_hook_start "$input"

# Change to hooks directory and run TypeScript analysis
HOOKS_DIR="${CLAUDE_PROJECT_DIR:-.}/.claude/hooks"
cd "$HOOKS_DIR"

# Run the skill analysis and display results
echo "$input" | npx tsx skill-activation-prompt.ts

log_hook_end "SUCCESS"
