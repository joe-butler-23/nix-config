#!/usr/bin/env bash
set -e

# Enhanced skill activation that combines hook enforcement with Claude's semantic abilities
# Instead of keyword matching, we prompt Claude to actively check for relevant skills

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Load logging utilities
source "$SCRIPT_DIR/logger.sh"

# Read input from stdin
input=$(cat)

# Log hook start
log_hook_start "enhanced-skill-prompt" "$input"

# Debug environment
log_info "enhanced-skill-prompt" "Environment check - CLAUDE_PROJECT_DIR: ${CLAUDE_PROJECT_DIR:-unset}"
log_info "enhanced-skill-prompt" "Current working directory: $(pwd)"
log_info "enhanced-skill-prompt" "Looking for skills at: $CLAUDE_PROJECT_DIR/.claude/skills"

# Check if skills directory exists
if [[ ! -d "$CLAUDE_PROJECT_DIR/.claude/skills" ]]; then
    log_info "enhanced-skill-prompt" "No skills directory found, skipping"
    log_hook_end "enhanced-skill-prompt" "SUCCESS"
    exit 0
fi

# Build available skills context
skills_context=""
skill_count=0
log_info "enhanced-skill-prompt" "Scanning skills directory: $CLAUDE_PROJECT_DIR/.claude/skills"

for skill_dir in "$CLAUDE_PROJECT_DIR/.claude/skills"/*; do
    if [[ -d "$skill_dir" ]] && [[ -f "$skill_dir/SKILL.md" ]]; then
        skill_name=$(basename "$skill_dir")
        # Extract description from YAML frontmatter
        description=$(head -20 "$skill_dir/SKILL.md" | grep "^description:" | cut -d':' -f2- | sed 's/^ *//')
        if [[ -n "$description" ]]; then
            skills_context+="- **$skill_name**: $description"$'\n'
            skill_count=$((skill_count + 1))
            log_info "enhanced-skill-prompt" "Loaded skill: $skill_name"
        else
            log_info "enhanced-skill-prompt" "Skipped skill $skill_name: no description found"
        fi
    else
        if [[ -d "$skill_dir" ]]; then
            log_info "enhanced-skill-prompt" "Skipped directory $(basename "$skill_dir"): no SKILL.md found"
        fi
    fi
done

log_info "enhanced-skill-prompt" "Total skills loaded: $skill_count"

# Only proceed if we have skills and this looks like a work prompt
if [[ -n "$skills_context" ]]; then
    log_info "enhanced-skill-prompt" "Displaying skill activation reminder"
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "🎯 SKILL ACTIVATION REMINDER"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "📚 AVAILABLE SKILLS:"
    echo "$skills_context"
    echo ""
    echo "💡 Claude: Check if any of these skills are relevant to the user's request."
    echo "   If so, mention that you'll use the relevant skill(s) and why."
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    log_hook_end "enhanced-skill-prompt" "SUCCESS"
else
    log_info "enhanced-skill-prompt" "No skills found, skipping display"
    log_hook_end "enhanced-skill-prompt" "SUCCESS"
fi
