# Setup Interview

Ask these questions up-front to avoid rework.

## Essentials

- Project name?
- What languages and build tools?
- Any repos/submodules?
- Do you need task tracking (Org-mode)?
  - If yes, where should tasks live (repo-local vs external)?

## Initialization Steps

- Initialize git if requested.
- Set up Nix environment (`flake.nix` or `shell.nix`).
- Add AI tooling layout (`.claude/` hooks/skills).
- If Org task tracking is desired, create or point to the task file and agree on TODO states.
