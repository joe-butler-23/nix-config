---
name: session-handoff
description: Mandatory workflow for ending AI coding sessions. Triggers will include phrases likes 'we are done here', 'let's finish up for the day', 'that's all for today' etc. Ensures all work is properly tracked, quality gates are met, and everything is synchronized before handoff.
---

# Session Handoff

**MANDATORY WORKFLOW** - Complete ALL steps before ending any coding session. The work is NOT finished until git push succeeds.

## Phase 1: File Remaining Work

If this repo is linked to a github repo then ensure all changes have been pushed. And then complete the beads shutdown process:

### Identify Incomplete Tasks
```bash
git status                  # Check for uncommitted changes
git diff                    # Review what's been modified
```

### Create Issues for Remaining Work
```bash
# File issues for any work that needs follow-up
bd create "Complete error handling in payment flow"
bd create "Add tests for new authentication logic"
bd create "Update documentation for API changes"
```

**Rule**: Never leave work undocumented. If you discover it, track it.

## Phase 2: Quality Gates

### Run Project-Specific Quality Checks
```bash
# Check what quality commands are available
cat .claude/tsc-cache/*/commands.txt 2>/dev/null || echo "No cached commands"

# Common quality gates (run as applicable):
npm run test                # Run tests
npm run lint               # Lint code
npm run typecheck          # TypeScript checking
npm run build              # Build verification
```

### Verify Changes Don't Break Build
```bash
# If working in monorepo/multi-service setup
cd frontend && npm run build
cd ../backend && npm test
cd ../database && npx prisma generate
```

## Phase 3: Update Status

### Close Completed Work
```bash
bd list                     # Review all open issues
bd close <issue-id>         # Mark completed work as done
```

### Update In-Progress Issues
```bash
bd status <issue-id> blocked    # If blocked on something
bd note <issue-id>              # Add progress notes
```

## Phase 4: Synchronization (NON-NEGOTIABLE)

### Git Synchronization
```bash
git pull --rebase
# Resolve conflicts if any
# For beads conflicts, usually safe to accept --theirs and re-import

bd sync                     # Synchronize beads database
git push                    # Push ALL work to remote
git status                  # MUST show "up to date"
```

**Critical Rules:**
- NEVER say "ready to push when you are!" - YOU must push
- If `git push` fails, retry until it succeeds
- Finishing means EVERYTHING is pushed. No exceptions.

### Clean Up Environment
```bash
git stash clear             # Clear any stashes
git remote prune origin     # Clean up remote references
```

## Phase 5: Learning Review & Capture

### Review Session Learnings
```bash
# Check for captured learning events
~/.claude/hooks/learning-capture.sh summary

# Review any pending skill improvements
echo "Learning events captured this session:"
if [[ -f "$HOME/.claude/learning/queue.jsonl" ]]; then
    grep '"status": "queued"' "$HOME/.claude/learning/queue.jsonl" | wc -l
    echo " learnings ready for review"
fi
```

### Process High-Priority Learnings
```bash
# Review high-priority learning events from this session
echo "High-priority learnings to address:"
if [[ -f "$HOME/.claude/learning/queue.jsonl" ]]; then
    grep '"priority": "high"' "$HOME/.claude/learning/queue.jsonl" | \
        jq -r '.learning_type + " - " + .context'
fi

# For each high-priority learning, consider immediate skill updates
# This is where Claude would propose specific skill file changes
```

### Create Learning Follow-up Issues
```bash
# Convert medium/low priority learnings to follow-up issues
echo "Creating follow-up issues for learning opportunities..."

# Example: Create beads issues for skill improvements
bd create "Skill improvement: Update local-development based on Docker networking session"
bd create "New skill candidate: Database operations and PostgreSQL patterns"
```

## Phase 6: Handoff Documentation

### Generate Next Session Prompt
```bash
# Check current context
current_branch=$(git symbolic-ref --short HEAD)
ready_issues=$(bd ready)

# Provide specific prompt for next session
echo "Next session prompt:"
if [[ "$current_branch" =~ bd-([0-9]+) ]]; then
    issue_id="${BASH_MATCH[1]}"
    echo "Continue work on bd-$issue_id. Run 'bd show $issue_id' for context."
else
    echo "Run 'bd ready' to see available work, then 'bd show <issue-id>' for specific tasks."
fi
```

## Troubleshooting

### Git Push Fails
```bash
# If push fails due to diverged history
git fetch origin
git rebase origin/main      # or appropriate branch
git push
```

### Beads Sync Conflicts
```bash
# If beads.jsonl conflicts during pull
git checkout --theirs .beads/beads.jsonl
bd import .beads/beads.jsonl
bd sync
```

### Quality Gates Fail
```bash
# Don't skip quality gates - fix the issues
npm run lint -- --fix      # Auto-fix linting issues
npm run test -- --watch    # Run tests in watch mode
# Fix failing tests, then retry
```
## Success Criteria

✅ **Session is complete when ALL of these are true:**
- No uncommitted changes OR remaining work filed as issues
- Quality gates have passed (tests, linting, builds)
- All completed work marked as closed in beads
- Learning events reviewed and processed (high-priority applied, others filed as issues)
- Git status shows "up to date" with remote
- Next session prompt provided

❌ **Session is NOT complete if ANY of these are true:**
- Uncommitted changes without corresponding issues
- Quality gates failing
- Git push pending or failed
- Beads out of sync with remote
