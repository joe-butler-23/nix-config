# Claude Code Skills System

Modular, self-contained packages that extend Claude's capabilities with specialized knowledge, workflows, and tool integrations.

## Overview

Skills transform Claude from a general-purpose agent into a specialized agent equipped with procedural knowledge for specific domains. Each skill provides:

1. **Specialized workflows** - Multi-step procedures for specific domains
2. **Tool integrations** - Instructions for working with specific formats or APIs
3. **Domain expertise** - Project-specific knowledge, schemas, business logic
4. **Bundled resources** - Scripts, references, and assets for complex tasks

## Skill Architecture

### Skill Structure
```
skill-name/
├── SKILL.md (required)
│   ├── YAML frontmatter (name + description)
│   └── Markdown instructions
└── Optional Resources
    ├── scripts/     - Executable code (Python/Bash/etc.)
    ├── references/  - Documentation loaded as needed
    └── assets/      - Templates, images, boilerplate
```

### Progressive Disclosure
Skills use three-level loading for context efficiency:
1. **Metadata** (name + description) - Always loaded
2. **SKILL.md body** - When skill triggers
3. **Resources** - As needed by Claude

## Active Skills

### Core Workflow Skills
- **session-handoff** - Quality gates and session completion checklists
- **self-learning** - Skill improvement and evolution patterns

### Development Skills
- **local-development** - NixOS + ZSH + Hyprland environment troubleshooting
- **project-initializer** - New project setup and configuration
- **documentation** - Technical writing and documentation patterns

### Creation Skills
- **skill-creator** - Guide for creating effective skills
- **writing-skills** - Content creation and writing workflows

## Skill Management

### Auto-Generated Files
- `available_skills.xml` - Generated skill catalog for Claude
- `generate-available-skills.sh` - Updates skill catalog

### Local Skills
- `local-skills/` - Project-specific or experimental skills
- Not included in main skill catalog

## Design Principles

### 1. Concise is Key
Context window is shared resource. Only include what Claude doesn't already know.

### 2. Appropriate Degrees of Freedom
- **High freedom** (text instructions) - Multiple valid approaches
- **Medium freedom** (pseudocode/scripts) - Preferred patterns with variation
- **Low freedom** (specific scripts) - Fragile operations requiring precision

### 3. Progressive Disclosure
Keep core instructions lean, move detailed content to references files.

### 4. Essential Only
Skills should contain only information needed for the AI agent to complete tasks.

## Usage Patterns

### Skill Activation
Skills are automatically suggested based on:
- YAML description matching user intent
- Context analysis by Claude
- Explicit skill requests (`use skill-name`)

### Skill Loading
1. All skill metadata always available to Claude
2. SKILL.md loaded when skill activates
3. References/scripts loaded as Claude determines need

### Skill Evolution
Skills improve through:
- Usage feedback and refinement
- Progressive disclosure optimization
- Resource organization improvements

This system provides Claude with domain expertise while maintaining context efficiency and allowing natural intelligence to guide skill selection and usage.
