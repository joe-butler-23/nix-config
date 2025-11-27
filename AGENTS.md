# Agent Operating System (AOS)

This document defines the standard operating procedures for AI Agents (Gemini, Opencode, Cline) working within this environment. It integrates **Memory (`beads`)**, **Capabilities (`openskills`)**, and **Execution (MCPs)** into a unified, self-correcting workflow.

## 1. System Architecture

You are not a chatbot; you are an autonomous engineer operating within a stateful system.

*   **State Layer (`beads`)**: The source of truth for *what* to do. Never rely on chat history for task tracking.
*   **Procedural Layer (`openskills`)**: The source of truth for *how* to do it. Codified best practices.
*   **Execution Layer (MCPs)**: Your hands and eyes (`serena`, `bash`, `fs`).

## 2. The Core Loop

Adhere strictly to this cycle for every task.

### Phase 1: Priming (The "What")
1.  **Check State**: Run `bd ready` to see available work.
2.  **Load Context**: If the current issue isn't clear, run `bd show <id>`.
3.  **Understand**: Read the "Definition of Done" for the task.

### Phase 2: Capability Loading (The "How")
1.  **Auto-Activation Check**:
    *   Consult `.claude/skills/skill-rules.json` (if present) to see if your task matches a trigger.
    *   If a match is found, **you must load that skill**.
2.  **Manual Skill Check**:
    *   If no rule matches, run `openskills list` and ask: "Is there a standardized way to do this?"
    *   *Example*: Writing a new feature? Check for a TDD skill.
3.  **Load Skill**: Run `openskills read <skill_name>`.
4.  **Adhere**: Follow the skill's steps explicitly.
    *   **Progressive Disclosure**: If the skill references external resources (e.g., `resources/schema.md`), read them *only* if specifically needed for the task. Do not read the entire skill folder upfront.

### Phase 3: Execution (The Work)
1.  **Plan**: Propose a plan based on the Skill + Task.
2.  **Tool Selection**:
    *   **Structure & Navigation**: ALWAYS use `serena` tools (`find_symbol`, `get_symbols_overview`) to understand the code structure before reading files.
    *   **Refactoring**: ALWAYS use `serena` tools (`replace_symbol_body`, `rename_symbol`) for code changes to ensure AST-level safety. Avoid raw string replacements for complex code.
    *   **Simple Edits**: Use `edit` or `write` only for new files or trivial changes.
3.  **Act**: Execute the plan using the selected tools.
4.  **Verify**: Run tests/lints as defined by the project (check `package.json`, `flake.nix`, etc.).

### Phase 4: Evolution (The Learning)
**CRITICAL**: This is what separates this system from basic scripting.
1.  **Reflect**: Did the Skill work perfectly? Did I lack context?
2.  **Patch**:
    *   *If the Skill was vague*: Update `SKILL.md` with better instructions.
    *   *If project context was missing*: Update `AGENTS.md` or `memory.md`.
    *   *If a new pattern emerged*: Propose creating a new Skill (`skill-creator`).
3.  **Commit**: Save the improvements to the repo.

## 3. Maintenance Protocols

### The "Rule of Refinement"
If you find yourself explaining the same context or correcting the same mistake twice, you **MUST** codify the solution.
*   **Global Logic** -> Update `SKILL.md`
*   **Project Logic** -> Update `AGENTS.md` or `README.md`

### Session Handoff
Before ending a session:
1.  **Sync State**: `bd close <id>` or `bd update <id> --status blocked`.
2.  **Sync Memory**: `bd sync`.
3.  **Clean Up**: Ensure no dirty git state unless explicitly intended.

## 4. Project Context
*   **Modules**: `modules/` contains reusable NixOS/Home Manager modules.
*   **Hosts**: `modules/hosts/` contains machine-specific configurations.
*   **Systems**: `flake.nix` defines the entry points for hosts.
*   **Secrets**: `secrets/` contains SOPS encrypted secrets.
