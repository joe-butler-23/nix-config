---
name: project-initializer
description: Interactive guide for setting up new projects with the AI AOS (Agent Operating System) toolkit. Use this when the user says "init project", "setup repo", or when entering a new repository that lacks standard tooling like `.beads` or `AGENTS.md`.
---

# Project Initializer

This skill guides the setup of a new project environment, ensuring it adheres to the Agent Operating System (AOS) standards. It covers Memory (Beads), Capabilities (Openskills), and Workflow (Agents/Opencode).

## Phase 1: Analysis & Interview

Before making changes, understand the terrain.

1.  **Inspect the Repository**:
    *   Run `ls -R` (or `tree` if available) to see the structure.
    *   Check for package managers: `package.json` (Node), `Cargo.toml` (Rust), `flake.nix` (Nix), `requirements.txt` (Python).
    *   Check for existing AI config: `.beads/`, `AGENTS.md`, `.opencode/`, `.claude/skills/`.

2.  **Interview the User**:
    *   Ask: "What is the primary goal of this project?"
    *   Ask: "Do you need specific MCP servers? (e.g., Postgres for DBs, Puppeteer for scraping)"
    *   Ask: "Should I initialize a local skills directory (`.claude/skills`) or rely on global skills?"

## Phase 2: Core Scaffolding

Once the plan is agreed upon, execute the standard setup.

### 1. Initialize Memory (Beads)
*   Check if `bd` is installed (`which bd`).
*   Run `bd init` in the project root.
*   Run `bd hooks install` (if git is initialized).
*   **Verify**: Ensure `.beads/` directory exists.

### 2. Create/Update `AGENTS.md`
This is the "Constitution" for the project. Create `AGENTS.md` with the following content (or append if it exists but lacks these sections):

```markdown
# Agent Operating System (AOS) Protocols

## 0. Onboarding
BEFORE ANYTHING ELSE: run 'bd onboard' (if available) or 'bd ready' to check for active tasks.

## 1. The Core Loop
1.  **Check Beads**: Always work off a specific issue ID.
2.  **Load Skills**: Check `openskills list` for relevant capabilities.
3.  **Execute**: Use tools to complete the work.
4.  **Sync**: Update Beads status before finishing.

## 2. Finishing Up
When the user says "let's finish up", you MUST complete ALL steps below. The work is NOT finished until git push succeeds.

1.  **File Follow-ups**: `bd create "Next task..."`
2.  **Quality Gates**: Run project tests/lints.
3.  **Update Issues**: `bd close <id>` or `bd update <id>`.
4.  **Sync & Push** (MANDATORY):
    *   `git pull --rebase`
    *   `bd sync`
    *   `git push`
    *   `git status` (Verify clean)
```

### 3. Opencode Integration
Symlink the standard configuration to ensure the project inherits the "Autonomous Engineer" agent profile.

*   **Command**: `ln -s ~/utilities/ai/opencode .opencode`
*   *Note*: Adjust the source path `~/utilities/ai` if the user's `utilities` repo is located elsewhere (check via `find ~ -name utilities -type d`).

## Phase 3: Environment Configuration

### 1. Nix Configuration (If `flake.nix` exists)
*   Check if `openskills` and `beads` are in the `devShell`.
*   If not, propose adding them.
*   **Example Flake Snippet**:
    ```nix
    devShells.default = pkgs.mkShell {
      buildInputs = [
        pkgs.beads      # Check actual package name in user's flake
        pkgs.openskills # Check actual package name
        # ... other inputs
      ];
    };
    ```

### 2. Gemini/MCP Configuration
If the user requested specific MCPs or the project type suggests them (e.g., Node.js project might benefit from a Node MCP):
*   Check for `.gemini/config.toml` or `extensions`.
*   Suggest adding the relevant MCP server configurations.

## Phase 4: Verification

1.  Run `bd doctor` (if available) to verify memory.
2.  Run `openskills list` to verify capabilities.
3.  Commit the setup changes: `git add .beads AGENTS.md .opencode && git commit -m "chore: initialize AOS project structure"` (Ask user before committing).
