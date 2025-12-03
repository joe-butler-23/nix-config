#!/usr/bin/env bash
set -e

# Beads workflow hook - manages beads task workflow with work capture and translation
# This runs on every UserPromptSubmit to capture work and integrate with beads

# Logging configuration
LOG_DIR="$HOME/.claude/logs"
LOG_FILE="$LOG_DIR/hooks.log"
BEADS_LOG="$LOG_DIR/beads.log"
HOOK_NAME="beads"

# Create log directory if it does not exist
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

log_beads() {
    local event_type="$1"
    local details="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')

    # Check if beads log rotation is needed (limit to 1000 lines)
    if [[ -f "$BEADS_LOG" ]]; then
        local line_count=$(wc -l < "$BEADS_LOG")
        if [[ $line_count -ge 1000 ]]; then
            mv "$BEADS_LOG" "${BEADS_LOG}.old"
        fi
    fi

    echo "[$timestamp] [$event_type] $details" >> "$BEADS_LOG"
    echo "[$timestamp] [$event_type] $details" >&2
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

# Log session start
log_beads "session-start" "Beads workflow initiated"

# Parse the user prompt for beads workflow decisions
prompt=$(echo "$input" | jq -r '.prompt // empty' | tr '[:upper:]' '[:lower:]')

# Only proceed with beads checks if:
# 1. bd command exists
# 2. We are in a beads-initialized project

if ! command -v bd &> /dev/null; then
    log_info "bd command not found, skipping beads workflow"
    log_beads "session-skip" "bd command not available"
    log_hook_end "SUCCESS"
    exit 0
fi

# Check if beads is initialized in this project
if [[ ! -f "$CLAUDE_PROJECT_DIR/.beads/config.yaml" && ! -f "$CLAUDE_PROJECT_DIR/.beads/config.json" ]]; then
    log_info "No .beads/config.yaml or .beads/config.json found, skipping beads workflow"
    log_beads "session-skip" "beads not initialized in project"
    log_hook_end "SUCCESS"
    exit 0
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🏗️  BEADS WORKFLOW INTEGRATION"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Show ready issues
ready_issues=$(bd ready 2>/dev/null || echo "")

if [[ -n "$ready_issues" ]]; then
    echo "📋 READY ISSUES:"
    echo "$ready_issues"
    echo ""
    echo "💡 Use 'bd show <issue-id>' to load context for specific issue"
    log_beads "ready-issues-shown" "$(echo "$ready_issues" | wc -l) ready issues available"
else
    echo "📋 NO READY ISSUES"
    echo "💡 Use 'bd create \"task description\"' to track work"
    log_beads "no-ready-issues" "No ready issues available"
fi

echo ""
echo "💡 BEADS WORKFLOW:"
echo "1. Create issue: bd create \"description\""
echo "2. Select issue: bd show <issue-id>"
echo "3. Track work through issue notes and updates"
echo ""

echo "📊 BEADS STATUS:"
echo "   Total issues: $(bd list --all 2>/dev/null | wc -l || echo '0')"
echo "   Ready to work: $(bd ready 2>/dev/null | wc -l || echo '0')"
echo "   In progress: $(bd list --status in-progress 2>/dev/null | wc -l || echo '0')"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

log_hook_end "SUCCESS"
