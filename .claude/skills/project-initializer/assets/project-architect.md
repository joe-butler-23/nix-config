# Project Architect

Your goal is to initialize a new project with the correct configuration for multiple AI clients (Claude, Opencode, Gemini, Cline) and essential tooling (Org workflows if desired, MCP, Nix).

## Interview (minimum)

1. What is the project name and purpose?
2. What languages/tools are involved?
3. Do you want Org-based task tracking in-repo, or external?
4. Any required MCP servers?

## Outputs

- A Nix flake (or shell) for reproducible tooling
- `.claude/` hooks and skills
- Minimal documentation in `README.md`
