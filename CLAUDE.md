# Project guidelines

[Project guidelines here]

# AI-Led Project Management

This document defines the standard operating procedures for AI Agents working within this repo. It integrates **Memory (`beads`)** and **Procedures (skills)** into a unified, self-correcting workflow that enforces good practices and a robust workflow. The user has a Nix Os machine with a zsh-p10k-kitty shell setup. Be vigilant to the fact you are running on a Nix machine and all that entails, in particular mainitaining a clean and tracked git tree at all times (or Nix may not be aware a file exists, or the changes made to it), and the need to use nix develop or nix-shell instead of npm/pip/etc install since it is a read only system. When working in this repo, you MUST work within the following system:

## Project Management Philosophy

This project uses an AI-first approach where Claude actively manages workflows rather than passively responding to requests. The system is built on three pillars:

### 1. Beads (bd) - Task Tracking
- **All work flows through beads issues** - no exceptions
- Issues created for any significant work (fixes, features, refactoring)
- Progress tracked through issue notes and status updates
- Git workflow optional (solo developer setup)

### 2. Skills - Domain Intelligence
- Specialized knowledge packages for specific workflows
- Progressive disclosure (metadata � instructions � detailed resources)
- AI-driven skill selection based on context and need
- Continuous evolution through usage and feedback
- You must be extremely vigilant about making use of the right skills at the right time.

### 3.Meta-Learning (Continuous Improvement)
**CRITICAL**: You should use built-in reflection and learning cycles for process improvement. There is a self-learning skill to enforce this. We do not want to just use the processes set out here, we want to ensure they are constantly evolving and improving.

1.  **Pattern Recognition**: When explaining same context twice, create new skill
2.  **Skill Improvement**: Update existing skills when gaps discovered
3.  **Process Evolution**: Use `skill-creator` for new workflow patterns
4.  **Metric Tracking**: Track skill usage and effectiveness over time

## Operational Workflow

### Session Start
1. Beads hook displays available issues and workflow guidance
2. Skills system activates relevant domain knowledge

### During Work
1. Select existing issue (`bd show <issue-id>`) or create new (`bd create "description"`)
2. Track progress through issue notes (`bd note <issue-id> "progress"`)
3. File changes automatically monitored and logged
4. Quality gates triggered for affected repositories

### Session End
1. Consider any self-learnings to incorporate into skills/documentation/etc
2. Session-handoff skill reviews progress and outstanding work
3. Issue status updated based on completion
4. Next session prepared with ready issues and context

## Key Principles

- **AI drives workflow** rather than following rigid processes. The aim is to leverage AI within strict guardrails.
- **Beads provides distributed issue tracking** without central servers. And ensures long-term memory
- **Progressive disclosure** keeps context lean while providing deep knowledge
- **Comprehensive audit trail** for all decisions and progress, and to confirm process operating as expected.

## Quick Commands

```bash
# Issue management
bd create "task description"        # Create new issue
bd ready                           # Show available work
bd show <issue-id>                 # Load issue context
bd note <issue-id> "progress"      # Track progress

# Skill management
use <skill-name>                   # Explicitly activate skill
skills                            # Show available skills

# Progress review
tail ~/.claude/logs/beads.log      # Recent activity
bd list --status in-progress      # Current work
```

This approach leverages AI's natural intelligence for workflow decisions while maintaining systematic progress tracking and knowledge management.
