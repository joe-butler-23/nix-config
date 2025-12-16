# scripts/file-launcher.nix
{pkgs}:
pkgs.writeShellScriptBin "fzf-file-launcher" ''
  set -euo pipefail

  # Configuration
  ALLOWED_DIRS=(
    "$HOME/downloads"
    "$HOME/documents"
    "$HOME/projects"
    "$HOME/development"
    "$HOME/nix-config"
  )

  EDITOR_CMD="${pkgs.xfce.mousepad}/bin/mousepad"

  IGNORE_DIRS=(
    ".git" "node_modules" "dist" "build" ".cache" "target" ".vscode" ".idea"
    "__pycache__" ".pytest_cache" "venv" "env" ".env" "coverage" ".coverage"
    "logs" "log" ".DS_Store" "Thumbs.db" ".obsidian/.trash" ".obsidian/cache"
    ".obsidian/workspace" "Archive"
  )

  # Function to list files
  list_files() {
    for dir in "''${ALLOWED_DIRS[@]}"; do
      [ -d "$dir" ] || continue
      ${pkgs.fd}/bin/fd -H -t f . "$dir" \
        --exclude .git --exclude node_modules --exclude dist --exclude build \
        --exclude .cache --exclude target --exclude .vscode --exclude .idea \
        --exclude __pycache__ --exclude .pytest_cache --exclude venv --exclude env \
        --exclude .env --exclude coverage --exclude .coverage --exclude logs \
        --exclude log --exclude .DS_Store --exclude Thumbs.db \
        --exclude .obsidian/.trash --exclude .obsidian/cache \
        --exclude .obsidian/workspace --exclude Archive \
        --follow
    done | ${pkgs.coreutils}/bin/sort -u
  }

  # Main file selection
  SELECTION="$(
    list_files | ${pkgs.fzf}/bin/fzf \
      --height=90% --layout=reverse --border \
      --prompt="Find file > " \
      --preview '${pkgs.bat}/bin/bat --style=numbers --color=always --line-range :200 {} 2>/dev/null || ${pkgs.coreutils}/bin/head -n 200 {}' \
      --preview-window=right:60%:wrap \
      --bind 'ctrl-y:execute(${pkgs.coreutils}/bin/printf "%s" {} | ${pkgs.wl-clipboard}/bin/wl-copy 2>/dev/null || ${pkgs.coreutils}/bin/printf "%s" {} | ${pkgs.xclip}/bin/xclip -selection clipboard 2>/dev/null || ${pkgs.coreutils}/bin/printf "%s" {} | ${pkgs.xsel}/bin/xsel --clipboard --input 2>/dev/null; echo "Copied: {}")+abort'
  )"

  [ -n "''${SELECTION:-}" ] || exit 0

  # Get MIME type
  mime="$(${pkgs.file}/bin/file --mime-type -Lb "$SELECTION" 2>/dev/null || echo "")"

  # Function to open detached
  open_detached() {
    ${pkgs.util-linux}/bin/setsid -f "$@" >/dev/null 2>&1 || true
  }

  # Check if footclient is available and foot server is running
  use_footclient() {
    command -v ${pkgs.foot}/bin/footclient >/dev/null 2>&1 && ${pkgs.procps}/bin/pgrep -x foot >/dev/null 2>&1
  }

  # Open file based on MIME type
  case "$mime" in
    text/*|application/x-shellscript|application/json|application/xml)
      editor_base="$(basename "$EDITOR_CMD")"
      case "$editor_base" in
        nvim|vim|vi|nano|hx|helix|kak|micro)
          # Terminal editors: need terminal wrapper
          if use_footclient; then
            open_detached ${pkgs.foot}/bin/footclient -a editor -D ~ "$EDITOR_CMD" "$SELECTION"
          elif command -v ${pkgs.foot}/bin/foot >/dev/null 2>&1; then
            open_detached ${pkgs.foot}/bin/foot -a editor -D ~ "$EDITOR_CMD" "$SELECTION"
          else
            term="''${TERMINAL:-${pkgs.xterm}/bin/xterm}"
            open_detached "$term" -e "$EDITOR_CMD" "$SELECTION"
          fi
          ;;
        *)
          # GUI editors: launch directly, no terminal wrapper
          open_detached "$EDITOR_CMD" "$SELECTION"
          ;;
      esac
      ;;
    *)
      if command -v ${pkgs.xdg-utils}/bin/xdg-open >/dev/null 2>&1; then
        open_detached ${pkgs.xdg-utils}/bin/xdg-open "$SELECTION"
      else
        echo "xdg-open not found (xdg-utils). Cannot open non-text file." >&2
        exit 1
      fi
      ;;
  esac

  # If running under Hyprland, try to close this picker window safely
  if command -v ${pkgs.hyprland}/bin/hyprctl >/dev/null 2>&1 && [ -n "''${HYPRLAND_INSTANCE_SIGNATURE:-}" ]; then
    if command -v ${pkgs.jq}/bin/jq >/dev/null 2>&1; then
      addr="$(${pkgs.hyprland}/bin/hyprctl -j activewindow 2>/dev/null | ${pkgs.jq}/bin/jq -r '.address' 2>/dev/null || true)"
      [ -n "$addr" ] && ${pkgs.hyprland}/bin/hyprctl dispatch closewindow "address:$addr" >/dev/null 2>&1 || true
    fi
  fi

  exit 0
''
