# Suggested Commands

## System Management (Aliases)
The project defines convenient aliases for common operations:
*   `ns`: Update system & user config (`sudo nixos-rebuild switch --flake .`)
*   `nsdry`: Test changes safely (`sudo nixos-rebuild dry-build --flake .`)

## Development & Quality
*   `nix fmt`: Format all Nix files in the repository.
*   `nix check`: Verify flake validity and formatting without applying changes.
*   `nix flake update`: Update all flake inputs.

## AI & Workflow
*   `bd ready`: Check for available tasks.
*   `bd create "Task description"`: Create a new task.
*   `bd list`: List all tasks.
*   `openskills list`: List available skills/procedures.
*   `openskills read <skill>`: Read a specific skill.

## File System
*   `ls -R`: Recursive list (useful for exploring module structure).
*   `grep -r "pattern" .`: Search for text patterns.
*   `find . -name "filename"`: Find files by name.
