---
name: beads-workflow
description: Comprehensive workflow for using beads (bd) task management system. Covers work capture, issue tracking, progress updates, and session handoff. Use when working with beads-enabled projects or when task management is needed.
---

# Beads Workflow

Essential patterns for capturing ALL work through the beads (bd) system with comprehensive logging.

## Core Principle: All Work Through Beads

**Every piece of work must be tracked through beads issues:**
- User requests → Translate to beads issues
- Recommendations → Create issues for follow-up
- Discoveries → Capture as new issues or notes
- Quick fixes → Still create issues (helps with history)

## Work Capture Patterns

### 1. Immediate Work (User Requested)
```bash
# For direct user requests
bd create "Fix login redirect bug" -t bug -p 1
git checkout -b bd-42-fix-login-redirect
bd update 42 -s in-progress
```

### 2. Discovered Work (Found During Current Work)
```bash
# Link to current work
bd create "Refactor auth module" -t enhancement -p 3
bd dep add 42 discovers 43  # Issue 42 discovered issue 43
```

### 3. Future Recommendations (Suggestions for Later)
```bash
bd create "Add user input validation" -t enhancement -p 2
bd update 44 -s ready  # Mark as ready for future work
```

### 4. Translation Examples
- **"Can you fix this bug?"** → `bd create "Fix [specific bug description]"`
- **"Let's refactor this"** → `bd create "Refactor [component] for [reason]"`
- **"Check if X works"** → `bd create "Investigate X functionality"`
- **"Add feature Y"** → `bd create "Implement Y feature"`

## Session Startup Workflow

### 1. Work Detection and Issue Creation
When beads hook detects work intent:
```bash
# Use suggested issue title or create custom
bd create "Implement user registration system" -t feature -p 2

# Create proper branch
git checkout -b bd-45-implement-user-registration

# Start work
bd update 45 -s in-progress
```

### 2. Continuing Existing Work
When on existing beads branch:
```bash
# Load context (automatic via hook)
bd show 42

# Resume work
echo "Continuing work on login redirect fix" | bd note 42
```

### 3. Using Existing Ready Issues
```bash
# Select from ready list (shown by hook)
bd show 38
git checkout -b bd-38-optimize-database-queries
bd update 38 -s in-progress
```

## During Work Tracking

### Track Progress Continuously
```bash
# Add significant progress notes
echo "Identified root cause in auth.js" | bd note 42
echo "Fixed validation logic, testing needed" | bd note 42
echo "Tests passing, ready for review" | bd note 42

# Update status as needed
bd update 42 -s testing
bd update 42 -s review
```

### Handle Discovered Work
```bash
# For blocking work
bd create "Fix database migration bug" -t bug -p 1
bd dep add 42 blocks-on 46  # Current work blocked by new issue

# For follow-up work
bd create "Add error handling to auth module" -t enhancement -p 3
bd dep add 46 follows 42  # New issue follows current work
```

### Create Sub-Issues for Complex Work
```bash
# Break down large issues
bd create "Design user registration UI" -t task -p 2
bd create "Implement registration backend" -t task -p 2
bd create "Add registration validation" -t task -p 2

# Link to parent
bd dep add 47 parent 45
bd dep add 48 parent 45
bd dep add 49 parent 45
```

## Issue Creation Best Practices

### Essential Information Template
```bash
# Complete issue with all context
bd create "Fix login redirect bug" \
  -d "Users redirected to /undefined after login instead of dashboard.
      Occurs on mobile Safari and Chrome.
      Error in auth.js line 42.

      ## Definition of Done
      - [ ] Login redirects to correct dashboard URL
      - [ ] Edge cases handled (no redirect, invalid URL)
      - [ ] Tests added for redirect logic
      - [ ] Manual testing completed on mobile" \
  -t bug \
  -p 1 \
  --labels "frontend,auth,mobile"
```

### Issue Types and Priorities
**Types**:
- **bug**: Something broken that needs fixing
- **feature**: New functionality to implement
- **enhancement**: Improvement to existing functionality
- **task**: Work item that's part of larger effort
- **epic**: Large work item spanning multiple issues

**Priorities**:
- **1**: Critical - blocks other work or production issue
- **2**: High - important for current milestone
- **3**: Medium - should be done but not urgent
- **4**: Low - nice to have, can wait

## Branch Integration

### Mandatory Branch Naming
**Pattern**: `bd-<issue-id>-<brief-description>`

```bash
# Examples
git checkout -b bd-42-fix-login-redirect
git checkout -b bd-18-add-user-search
git checkout -b bd-7-refactor-auth-module
```

### Branch Lifecycle
```bash
# Start work
bd create "Task description"
git checkout -b bd-50-task-description
bd update 50 -s in-progress

# During work
git commit -m "Progress on issue 50: implement core logic"
echo "Core logic implemented" | bd note 50

# Complete work
git commit -m "Complete issue 50: task finished"
bd update 50 -s complete
bd close 50
```

## Beads Logging System

All beads activities are automatically logged to `~/.claude/logs/beads.log`:

### Automatic Events (Logged by Hooks)
- `session-start`: Beads workflow begins
- `work-detected`: AI detects work intent in prompt
- `issue-context-loaded`: Existing issue loaded from branch
- `session-skip`: Session skipped (simple question, etc.)
- `ready-issues-shown`: Ready issues displayed
- `untracked-work-warning`: Work detected without beads tracking

### Manual Events (Log When You Do These Actions)
- `issue-created`: New issue created
- `issue-selected`: Existing issue chosen for work
- `work-started`: Beginning work on issue
- `progress-updated`: Progress note added
- `issue-closed`: Issue completed

### Viewing Beads Activity
```bash
# Recent beads activity
tail -20 ~/.claude/logs/beads.log

# Today's activity
grep "$(date '+%Y-%m-%d')" ~/.claude/logs/beads.log

# Filter specific events
grep "issue-created" ~/.claude/logs/beads.log
grep "work-detected" ~/.claude/logs/beads.log
grep "progress-updated" ~/.claude/logs/beads.log

# Session summaries
grep "session-summary" ~/.claude/logs/beads.log | tail -5
```

## Troubleshooting Common Issues

### Sync Conflicts
```bash
# If beads sync conflicts occur
bd export > backup.jsonl         # Backup current state
bd sync --force                  # Force sync (careful!)
# OR
bd sync --resolve-conflicts      # Interactive resolution
```

### Lost Issue Recovery
```bash
bd list --all                   # See all issues including closed
bd search "login redirect"      # Search by content
bd show <issue-id> --history    # Show issue history
```

### Branch/Issue Mismatch
```bash
# If branch doesn't match issue
current_branch=$(git branch --show-current)
echo "Branch $current_branch created for different work" | bd note <issue-id>

# Create proper branch
git checkout -b bd-<correct-issue-id>-proper-description
```

## Quality Gates and Session Handoff

### Pre-Completion Checklist
Before closing any issue:
```bash
# Verify definition of done
bd show <issue-id>  # Review original requirements

# Add completion notes
echo "All requirements met, tests passing" | bd note <issue-id>

# Update and close
bd update <issue-id> -s complete
bd close <issue-id>
```

### Session End Requirements
1. All active work must be in beads issues
2. Current branch must reference valid issue
3. Issue status must reflect current state
4. Progress notes must be up to date
5. Next session must have ready issues available

This workflow ensures comprehensive tracking of all development activity through the beads system with full audit trails and logging.
