#!/usr/bin/env bash
# Learning capture infrastructure for real-time AI learning detection

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Load logging utilities
source "$SCRIPT_DIR/logger.sh"

# Learning capture directory
LEARNING_DIR="$HOME/.claude/learning"
LEARNING_QUEUE="$LEARNING_DIR/queue.jsonl"

# Ensure learning directory exists
mkdir -p "$LEARNING_DIR"

# Function to capture learning events
capture_learning_event() {
    local trigger="$1"
    local context="$2"
    local learning_type="$3"
    local priority="${4:-medium}"
    local skill_target="$5"

    local timestamp=$(date -Iseconds)
    local session_id="${SESSION_ID:-$(date +%s)}"

    # Create learning event JSON
    local learning_event=$(cat <<EOF
{
  "timestamp": "$timestamp",
  "session_id": "$session_id",
  "trigger": "$trigger",
  "context": "$context",
  "learning_type": "$learning_type",
  "priority": "$priority",
  "skill_target": "$skill_target",
  "status": "pending"
}
EOF
)

    # Append to queue
    echo "$learning_event" >> "$LEARNING_QUEUE"

    log_message "learning-capture" "Learning event captured: $learning_type ($priority priority)"

    # High priority learnings get immediate notification
    if [[ "$priority" == "high" ]]; then
        echo "💡 High-priority learning captured: $learning_type" >&2
    fi
}

# Function to perform AI-driven micro-reflection on tool usage
claude_micro_reflection() {
    local tool_name="$1"
    local file_path="$2"
    local recent_context="$3"

    # Simple heuristics for now - can be enhanced with actual Claude calls
    # Look for patterns that suggest learning opportunities

    local learning_detected="false"
    local learning_type=""
    local skill_target=""

    # Pattern: Multiple edits to same file in short time (iteration)
    if [[ -n "$file_path" ]]; then
        local edit_count=$(grep "$file_path" "$HOME/.claude/logs/hooks.log" | grep "post-tool-use-tracker" | tail -10 | wc -l)
        log_message "learning-capture" "DEBUG: Edit count for $file_path: $edit_count"
        if [[ $edit_count -gt 2 ]]; then
            learning_detected="true"
            learning_type="iterative_refinement"
            skill_target="workflow_optimization"
            log_message "learning-capture" "DEBUG: Detected iterative refinement for $file_path"
        fi
    fi

    # Pattern: Tool usage on skill-related files (skill evolution)
    if [[ "$file_path" =~ \.claude/skills/ ]]; then
        learning_detected="true"
        learning_type="skill_modification"
        skill_target=$(echo "$file_path" | grep -o '\.claude/skills/[^/]*' | cut -d'/' -f3)
    fi

    # Pattern: Error-prone operations (bash failures, etc.)
    if echo "$recent_context" | grep -qi "error\|failed\|exception"; then
        learning_detected="true"
        learning_type="error_recovery"
        skill_target="local-development"
    fi

    echo "$learning_detected:$learning_type:$skill_target"
}

# Function to check for skill gaps before responses
skill_gap_detection() {
    local user_request="$1"

    # Detect patterns that might indicate skill gaps
    local gap_detected="false"
    local gap_type=""

    # New technology mentions
    if echo "$user_request" | grep -qi "kubernetes\|docker\|terraform\|ansible"; then
        local has_relevant_skill=$(find "$CLAUDE_PROJECT_DIR/.claude/skills" -name "*.md" -exec grep -l "docker\|kubernetes" {} \; | wc -l)
        if [[ $has_relevant_skill -eq 0 ]]; then
            gap_detected="true"
            gap_type="infrastructure_tooling"
        fi
    fi

    # Database operations
    if echo "$user_request" | grep -qi "database\|sql\|postgres\|mysql"; then
        local has_db_skill=$(find "$CLAUDE_PROJECT_DIR/.claude/skills" -name "*.md" -exec grep -l "database\|sql" {} \; | wc -l)
        if [[ $has_db_skill -eq 0 ]]; then
            gap_detected="true"
            gap_type="database_operations"
        fi
    fi

    echo "$gap_detected:$gap_type"
}

# Function to get recent conversation context (simplified)
get_recent_context() {
    # This would ideally extract recent conversation context
    # For now, return basic info from recent logs
    tail -20 "$HOME/.claude/logs/hooks.log" | grep -E "(tool|skill|error)" | tail -5
}

# Function to process learning queue
process_learning_queue() {
    if [[ ! -f "$LEARNING_QUEUE" ]]; then
        return 0
    fi

    local pending_learnings=$(grep '"status": "pending"' "$LEARNING_QUEUE" | wc -l)

    if [[ $pending_learnings -gt 0 ]]; then
        log_message "learning-capture" "Processing $pending_learnings pending learning events"

        # For now, just mark as processed - actual processing happens during session handoff
        sed -i 's/"status": "pending"/"status": "queued"/' "$LEARNING_QUEUE"

        echo "📚 $pending_learnings learning events queued for session review" >&2
    fi
}

# Function to view learning summary
view_learning_summary() {
    if [[ ! -f "$LEARNING_QUEUE" ]]; then
        echo "No learning events captured yet."
        return 0
    fi

    echo "Recent Learning Events:"
    echo "====================="

    # Process the entire file as multi-line JSON objects
    local event_buffer=""
    local brace_count=0

    while IFS= read -r line; do
        event_buffer+="$line"

        # Count braces to detect complete JSON objects
        brace_count=$((brace_count + $(echo "$line" | grep -o '{' | wc -l)))
        brace_count=$((brace_count - $(echo "$line" | grep -o '}' | wc -l)))

        # When braces balance, we have a complete JSON object
        if [[ $brace_count -eq 0 ]] && [[ -n "$event_buffer" ]]; then
            # Parse the complete JSON object
            local timestamp=$(echo "$event_buffer" | jq -r '.timestamp // "unknown"' 2>/dev/null || echo "unknown")
            local learning_type=$(echo "$event_buffer" | jq -r '.learning_type // "unknown"' 2>/dev/null || echo "unknown")
            local priority=$(echo "$event_buffer" | jq -r '.priority // "medium"' 2>/dev/null || echo "medium")
            local status=$(echo "$event_buffer" | jq -r '.status // "unknown"' 2>/dev/null || echo "unknown")
            local context=$(echo "$event_buffer" | jq -r '.context // ""' 2>/dev/null || echo "")

            printf "[%s] %s (%s priority) - %s\n" \
                "$(echo "$timestamp" | cut -d'T' -f1)" \
                "$learning_type" \
                "$priority" \
                "$status"

            if [[ -n "$context" && ${#context} -gt 0 ]]; then
                echo "    Context: ${context:0:80}..."
            fi

            # Reset for next object
            event_buffer=""
        fi
    done < "$LEARNING_QUEUE"
}

# If called directly, show learning summary
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-summary}" in
        "summary")
            view_learning_summary
            ;;
        "process")
            process_learning_queue
            ;;
        *)
            echo "Usage: $0 [summary|process]"
            ;;
    esac
fi
