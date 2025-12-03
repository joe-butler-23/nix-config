# Project guidelines

This repository manages the NixOS configuration for optimal system stability, performance, and reproducibility. All contributions and modifications must adhere to the following principles:

## 1. Modularity and Organization
- **Flake-based Structure**: Maintain a well-organized `flake.nix` and `flake.lock` for reproducible builds.
- **Module Separation**: Group related configurations into distinct Nix modules (e.g., `modules/desktop`, `modules/apps`).
- **Home Manager Integration**: Utilize Home Manager for user-specific configurations, ensuring separation from system-wide settings.

## 2. Best Practices
- **Nix Purity**: Prioritize pure functions and avoid imperative shell scripting within Nix expressions where declarative alternatives exist.
- **Immutability**: Embrace the immutable nature of NixOS. Avoid direct system modifications; all changes must be reflected in the configuration.
- **Declarative Approach**: Strive for a fully declarative system configuration.

## 3. Security and Secrets Management
- **SOPS Integration**: All sensitive information (passwords, API keys) must be managed using `sops-nix` and encrypted. Never commit unencrypted secrets.
- **Least Privilege**: Configure services and applications with the minimum necessary permissions.

## 4. Documentation and Comments
- **Self-Documenting Code**: Write clear, concise Nix expressions.
- **Module Readmes**: Each significant module should have a `README.md` explaining its purpose and configurable options.
- **Inline Comments**: Use comments sparingly to explain complex logic or non-obvious choices, focusing on "why" rather than "what."

## 5. Testing and Validation
- **Local Builds**: Always test configuration changes locally using `nixos-rebuild switch --flake .#yourhostname` or `home-manager switch --flake .#yourusername` before committing.
- **Type Checking**: Before adding new Nix options, verify expected types using `nixos-option` or documentation to prevent build errors.
- **System Capability Check**: Before adding custom configurations, check if NixOS already provides the required functionality out-of-the-box.
- **Linting**: Ensure Nix expressions are formatted and linted (e.g., using `nixpkgs-fmt` or `alejandra`).

## 6. Version Control
- **Atomic Commits**: Each commit should represent a single, logical change.
- **Descriptive Messages**: Write clear and concise commit messages.
- **Regular Updates**: Keep `flake.lock` up-to-date with upstream `nixpkgs` or other dependencies.

# AI-Led Project Management

**Core Principle: All work should be tracked through beads issues and guided by skills.**

## Project Management Philosophy

This document defines the standard operating procedures for AI Agents working within this repo. It integrates **Memory (`beads`)** and **Procedures (skills)** into a unified, self-correcting workflow that enforces good practices and a robust workflow. The user has a Nix Os machine with a zsh-p10k-kitty shell setup. Be vigilant to the fact you are running on a Nix machine and all that entails, in particular mainitaining a clean and tracked git tree at all times (or Nix may not be aware a file exists, or the changes made to it), and the need to use nix develop or nix-shell instead of npm/pip/etc install since it is a read only system. When working in this repo, you MUST work within the following system:

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
