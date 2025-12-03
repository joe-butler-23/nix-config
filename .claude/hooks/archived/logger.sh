#!/usr/bin/env bash
# Centralized logging utility for Claude hooks

LOG_DIR="$HOME/.claude/logs"
LOG_FILE="$LOG_DIR/hooks.log"

# Create log directory if it doesn't exist
mkdir -p "$LOG_DIR"

# Function to log messages
log_message() {
    local hook_name="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')

    # Check if log rotation is needed (limit to 1000 lines)
    if [[ -f "$LOG_FILE" ]]; then
        local line_count=$(wc -l < "$LOG_FILE")
        if [[ $line_count -ge 1000 ]]; then
            # Rotate the log file
            mv "$LOG_FILE" "${LOG_FILE}.old"
        fi
    fi

    echo "[$timestamp] [$hook_name] $message" >> "$LOG_FILE"

    # Also log to stderr so it appears in Claude Code's debug output
    echo "[$timestamp] [$hook_name] $message" >&2
}

# Function to log hook start
log_hook_start() {
    local hook_name="$1"
    local input_preview="${2:0:100}"  # First 100 chars of input

    log_message "$hook_name" "STARTED - Input preview: $input_preview"
}

# Function to log hook end
log_hook_end() {
    local hook_name="$1"
    local status="$2"  # SUCCESS or ERROR

    log_message "$hook_name" "FINISHED - Status: $status"
}

# Function to log general info
log_info() {
    local hook_name="$1"
    local info="$2"

    log_message "$hook_name" "INFO: $info"
}

# Function to view recent logs
view_recent_logs() {
    if [[ -f "$LOG_FILE" ]]; then
        echo "Recent Claude hook activity:"
        tail -20 "$LOG_FILE"
    else
        echo "No hook logs found at $LOG_FILE"
    fi
}

# If called directly, show recent logs
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    view_recent_logs
fi
