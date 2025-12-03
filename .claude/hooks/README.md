# Claude Code Hooks System

Streamlined workflow enforcement through essential hooks with intelligent skill integration.

## System Overview

This hooks system provides focused workflow automation. The primary function is to more strictly enforce workflows/processes than a simple CLAUDE.md file is likely to achieve. In particular, we use hooks for:

1. **Task Management**: Beads workflow integration and issue tracking
2. **Session Management**: Skill reminders and workflow enforcement
3. **Build Integration**: File tracking and quality gate automation
4. **Comprehensive Logging**: Track hook activity and decisions for debugging

## File Structure

### Active Hooks
- `beads.sh` - Beads workflow integration for task management (UserPromptSubmit)
- `session-startup.sh` - Skill activation reminders, skill usage logging (UserPromptSubmit)
- `post-tool-use-tracker.sh` - File edit tracking and build automation (PostToolUse)

### Archived Hooks
There is also a directory for archived hooks. These may or may not work.

### Log Locations
- **Hook logs**: `~/.claude/logs/hooks.log` (auto-rotated at 1000 lines)
- **Beads logs**: `~/.claude/logs/beads.log` (task management activities)
- **Debug output**: Hooks also write to stderr (visible in Claude Code debug output)

## Active Hooks

### 1. beads (UserPromptSubmit) - Beads workflow integration
- **Purpose**: Task management and issue tracking workflow
- **Functions**:
  - Shows ready beads issues at session start
  - Provides clean workflow guidance (create/select/track)
  - Logs all activities to dedicated beads.log
  - AI-driven rather than regex-based detection

### 2. session-startup (UserPromptSubmit) - Skill activation
- **Purpose**: Skill activation, usage logging, and session initialization
- **Functions**:
  - Displays skill activation reminder with available skills
  - Provides skill descriptions to guide Claude's decisions
  - Provides skill usage logging functions for tracking actual usage

### 3. post-tool-use-tracker (PostToolUse) - Build automation
- **Purpose**: Build system integration and file tracking
- **Functions**:
  - Tracks edited files and affected repositories
  - Generates build/typecheck commands for quality gates
  - Caches commands for execution
  - Integrates with quality assurance workflows

## Architecture Principles

**Core Design Principles**:

I have a hook to enforce skill-loading (based heavily on https://github.com/diet103/claude-code-infrastructure-showcase) and try and manage everything else through skills where possible. The principles to adhere to are:

1. **Simplicity First**: Working hard to achieve simplicity - minimal, focused hooks
2. **Clear Separation**: Hooks = workflow triggers; Skills = intelligent analysis
3. **Essential Only**: Only includes workflow enforcement that can't be handled by skills
4. **Maintainable**: Easy to understand, debug, and modify

The system enforces consistent workflows while allowing Claude's natural intelligence to handle learning and skill evolution.
