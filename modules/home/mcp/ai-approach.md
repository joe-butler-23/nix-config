# Initiate project

Create an overarching AGENTS.md file then initiate a beads project at the start of a new project, using curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash to install, and then running the following:

```
# In your project root:
bd init

# bd will:
# - Create .beads/ directory with database
# - Import existing issues from git (if any)
# - Prompt to install git hooks (recommended: say yes)
# - Prompt to configure git merge driver (recommended: say yes)
# - Auto-start daemon for sync

# Then tell your agent about bd:
echo "BEFORE ANYTHING ELSE: run 'bd onboard' and follow the instructions" >> AGENTS.md

# Setup git hooks - https://github.com/steveyegge/beads/tree/main/examples/git-hooks

bd hooks installl

```

# AGENTS.md

- Be sure that all the bd stuff is properly included
-Add a session-ending protocol. This should include something like (adapt according to specific project/workflow):

```
FINISHING UP

When the user says "let's finish up", you MUST complete ALL steps below. The work is NOT finished until git push succeeds. NEVER stop before pushing. NEVER say "ready to push when you are!" - that is a FAILURE.

MANDATORY WORKFLOW - COMPLETE ALL STEPS:

1. File beads issues for any remaining work that needs follow-up

2. Ensure all quality gates pass (only if code changes were made) - run tests, linters, builds (file P0 issues if broken)

3. Update beads issues - close finished work, update status

4. PUSH TO REMOTE - NON-NEGOTIABLE - This step is MANDATORY. Execute ALL commands below:

# Pull first to catch any remote changes
git pull --rebase

# If conflicts in .beads/beads.jsonl, resolve thoughtfully:
#   - git checkout --theirs .beads/beads.jsonl (accept remote)
#   - bd import -i .beads/beads.jsonl (re-import)
#   - Or manual merge, then import

# Sync the database (exports to JSONL, commits)
bd sync

# MANDATORY: Push everything to remote
# DO NOT STOP BEFORE THIS COMMAND COMPLETES
git push

# MANDATORY: Verify push succeeded
git status  # MUST show "up to date with origin/main"

CRITICAL RULES:

The work has NOT finished until git push completes successfully
NEVER stop before git push - that leaves work stranded locally
NEVER say "ready to push when you are!" - YOU must push, not the user. Do not create confusion as to whether it has been pushed or not.
If git push fails, resolve the issue and retry until it succeeds
The user is managing multiple agents - unpushed work breaks their coordination workflow
Clean up git state - Clear old stashes and prune dead remote branches:

git stash clear                    # Remove old stashes
git remote prune origin            # Clean up deleted remote branches

5. Verify clean state - Ensure all changes are committed AND PUSHED, no untracked files remain

6. Choose a follow-up issue for next session

- Provide a prompt for the user to give to you in the next session
- Format: "Continue work on bd-X: [issue title]. [Brief context about what's been done and what's next]"

REMEMBER: Finishing up for the day means EVERYTHING is pushed to remote. No exceptions.

Example "finish up" session:

# 1. File remaining work
bd create "Add integration tests for sync" -t task -p 2 --json

# 2. Run quality gates (only if code changes were made)
go test -short ./...
golangci-lint run ./...

# 3. Close finished issues
bd close bd-42 bd-43 --reason "Completed" --json

# 4. PUSH TO REMOTE - MANDATORY, NO STOPPING BEFORE THIS IS DONE
git pull --rebase
# If conflicts in .beads/beads.jsonl, resolve thoughtfully:
#   - git checkout --theirs .beads/beads.jsonl (accept remote)
#   - bd import -i .beads/beads.jsonl (re-import)
#   - Or manual merge, then import
bd sync        # Export/import/commit
git push       # MANDATORY
git status     # MUST verify "up to date with origin/main"

# 5. Clean up git state
git stash clear
git remote prune origin

# 6. Verify everything is clean and pushed
git status

# 7. Choose next work
bd ready --json
bd show bd-44 --json

# FINAL STEPS

When finished up, provide the user with:

- Summary of what was completed this session
- What issues were filed for follow-up
- Status of quality gates (all passing / issues filed)
- Confirmation that ALL changes have been pushed to remote
- Recommended prompt for next session
```