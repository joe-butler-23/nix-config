# Custom Utility Scripts

This module contains a collection of Nix-managed shell scripts designed to enhance workflow efficiency, system maintenance, and focus. These scripts are packaged as binaries and available in the user's path.

## 🚀 Launchers & Navigation

### `fzf-file-launcher`
**Source:** `file-launcher.nix`

A fast, keyboard-centric file finder and opener using `fd` and `fzf`.
- **Features:**
  - searches specific directories (`~/bin`, `~/Downloads`, `~/Documents`, `~/development`, `~/nix-config`, `~/org`)
  - Intelligent ignoring of build artifacts and cache directories
  - File previews with `bat`
  - "Copy to clipboard" binding (`Ctrl-Y`)
  - Smart opening: Launches GUI apps directly or terminal editors (vim, nano) inside a terminal wrapper (preferring `footclient` if available)

### `recent-files-launcher`
**Source:** `recent-files-launcher.nix`

Quickly access recently used files based on the GTK recent files database (`~/.local/share/recently-used.xbel`).
- **Features:**
  - Sorts by most recently visited
  - `fzf` interface with file previews
  - Handles mime-types to open files with the appropriate application

### `directory-finder`
**Source:** `directory-finder.nix`

Rapidly navigate directories and copy their absolute paths to the clipboard.
- **Usage:** `directory-finder [start_dir]` (defaults to current directory)
- **Features:**
  - Recursive search using `fd`
  - Copies selected path to system clipboard (Wayland `wl-copy`)

## 🧠 Workflow & Focus

### `copy-prompt`
**Source:** `copy-prompt.nix`

A utility for managing and using a library of AI system prompts.
- **Config:** Looks for text/markdown files in `~/Documents/prompting`
- **Features:**
  - Select a prompt file via `fzf` with preview
  - Automatically copies content to clipboard
  - Supports both Wayland (`wl-copy`) and X11 (`xclip`)

### `study-focus`
**Source:** `study-focus.nix`

A distraction-blocking mode for deep work sessions.
- **Usage:** `study-focus [on|off|toggle]`
- **Function:**
  - Aggressively kills blocked applications (Firefox, Chrome, Brave) while active
  - Uses a lockfile (`/tmp/study-focus-state`) to maintain state
  - Sends system notifications on state change

## 🛠️ System Maintenance

### `weekly-review`
**Source:** `maintenance/weekly-review.nix`

An interactive Terminal User Interface (TUI) for routine system maintenance, built with `gum` and styled with Nord colors.

- **Menu Options:**
  - **sys_update:** Updates flake inputs, checks `flake.lock` changes, commits changes, and runs `nixos-rebuild switch`. Each step requires manual confirmation for safety.
  - **clean_recent_files:** Scans specific directories for files changed in the last 7 days and offers a multi-select interface to delete them.
  - **ai_system_review:** Gathers comprehensive system diagnostics (system info, failed units, disk usage, network status, journal errors/warnings) and opens an interactive Claude Code session for analysis. User can discuss findings, ask follow-up questions, and take actions directly with Claude's assistance. Diagnostics cached in `~/.cache/weekly-review/`.

- **Features:**
  - Nord color palette throughout for consistent UX
  - Automatic weekly garbage collection configured at system level (no manual GC needed)
  - Diagnostic history kept (last 10 runs)
  - Interactive Claude Code integration for intelligent troubleshooting and remediation
