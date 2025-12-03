# Project Setup Interview Protocol

The Project Architect agent must follow this interview process to gather requirements before generating configurations.

## Phase 1: Discovery

1.  **Project Context**:
    *   "What is the name of this project?"
    *   "What is the primary programming language or framework?" (e.g., Node.js, Python, Rust, Generic)
    *   "Briefly describe the project's goal."

2.  **Tooling Requirements**:
    *   "Do you need task tracking (Beads)?" (Yes/No)
    *   "Do you need Nix environment (flake.nix)?" (Yes/No)

3.  **MCP Server Selection**:
    *   "Which MCP servers does this project require?"
        *   *Examples: Postgres, Git, Filesystem, Linear, Github, Anki*
    *   "Are there any custom MCP servers?"

## Phase 2: Confirmation

*   Summarize the gathered requirements.
*   "I am about to generate the configuration files for [List Clients]. Proceed?"

## Phase 3: Execution (Internal)

1.  **Drafting**:
    *   Update `AGENTS.md` and `GEMINI.md` and `CLAUDE.md` with project context and make sure they are all aligned.
    *   Create `flake.nix` with language dependencies if needed
    *   For each selected client, draft the specific config files (`.claude/settings.json`, `opencode.json`, etc.) ensuring MCP servers are correctly defined per client spec.
    *   Create standard directory structures (`.claude/agents`, `.opencode/command`, etc.).

2.  **Writing**:
    *   Write all files to disk.
    *   Initialize git/beads if requested.
    *   Run `bd init` if beads is selected.
    *   Run `bd onboard --output .beads/BD_GUIDE.md` if beads is initialized.
    *   Run `openskills sync` to inject skill documentation into `AGENTS.md`.

## Phase 4: Handoff

*   "Project [Name] has been initialized."
*   "Please review `AGENTS.md` to add specific quality gates."
*   "Run `bd ready` to see your first task."
