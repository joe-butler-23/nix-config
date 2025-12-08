{
  pkgs,
  ...
}: let
  # The actual Brave Browser package
  realBrave = pkgs.brave;

  # The Wrapper Script
  braveGatekeeper = pkgs.writeShellScriptBin "brave" ''
    # Ensure dependencies are in PATH
    export PATH="${pkgs.gum}/bin:${pkgs.coreutils}/bin:${pkgs.findutils}/bin:${pkgs.kitty}/bin:$PATH"

    # Configuration
    # We hardcode the path because this script lives in the read-only Nix store
    PROJECTS_DIR="$HOME/documents/projects"
    HOOKS_DIR="$HOME/documents/projects/sys-arc/project-hooks"

    # Nord Colors
    NORD11="#BF616A"
    NORD13="#EBCB8B"
    NORD14="#A3BE8C"
    NORD15="#B48EAD"
    NORD6="#ECEFF4"

    # --- 1. Terminal Detection ---
    # If not running in a terminal, re-launch self inside Kitty
    if [ ! -t 0 ]; then
       exec kitty --class brave-gatekeeper -e "$0" "$@"
    fi

    # --- 2. Interface ---
    gum style --foreground "$NORD6" --padding "1 2" "Focusing on..."

    # Get projects
    if [ -d "$PROJECTS_DIR" ]; then
        PROJECTS=$(find "$PROJECTS_DIR" -mindepth 1 -maxdepth 1 -type d -not -name '.*' -printf "%f\n" | sort)
    else
        PROJECTS=""
    fi

    OPTIONS="''${PROJECTS}
    browse"

    CHOICE=$(echo "$OPTIONS" | gum choose \
        --header "" \
        --cursor.foreground "$NORD13" \
        --selected.foreground "$NORD13" \
        --height 10)

    # --- 3. Handle Choice ---
    if [[ "$CHOICE" == "browse" ]]; then
        gum style --foreground "$NORD13" "authenticate"

        # We use /run/wrappers/bin/sudo explicitly if needed, but 'sudo' should be in path
        PASSWORD=$(gum input --password --placeholder "sudo password..." --prompt.foreground "$NORD13" --cursor.foreground "$NORD13")

        if echo "$PASSWORD" | sudo -S -k true 2>/dev/null; then
            gum style --foreground "$NORD15" "✓ launching brave.."

            # Launch Real Brave
            # We use nohup to detach it from this shell
            nohup "${realBrave}/bin/brave" "$@" >/dev/null 2>&1 &
            sleep 0.5
            exit 0
        else
            gum style --foreground "$NORD11" "✗ password failed"
            sleep 1
            exit 1
        fi
    else
        # Project Selected
        gum style --foreground "$NORD14" "✓ focusing on $CHOICE."

        HOOK_SCRIPT="$HOOKS_DIR/$CHOICE.sh"
        if [ -f "$HOOK_SCRIPT" ] && [ -x "$HOOK_SCRIPT" ]; then
            "$HOOK_SCRIPT"
        fi

        sleep 1
        exit 0
    fi
  '';
in {
  # IMPORTANT: REMOVE `pkgs.brave` from systemPackages to avoid collisions.
  environment.systemPackages = [braveGatekeeper];
}
