{pkgs, ...}: let
  realBrave = pkgs.brave;

  braveGatekeeper = pkgs.writeShellScriptBin "brave" ''
    set -euo pipefail

    # Ensure dependencies are in PATH
    export PATH="${pkgs.gum}/bin:${pkgs.coreutils}/bin:${pkgs.findutils}/bin:${pkgs.sudo}/bin:${pkgs.kitty}/bin:${pkgs.emacs}/bin:${pkgs.libnotify}/bin:${pkgs.gnugrep}/bin:${pkgs.gawk}/bin:${pkgs.util-linux}/bin:${pkgs.systemd}/bin:${pkgs.hyprland}/bin:$PATH"

    # === CONFIGURATION ===
    readonly PROJECTS_DIR="$HOME/documents/projects"
    readonly HOOKS_DIR="$PROJECTS_DIR/sys-arc/project-hooks"
    readonly LOG_FILE="$HOME/documents/sys-arc/browsing-log.org"
    readonly REQUIRED_PASS="wastingtime"

    # Capture original arguments (URLs) immediately
    BRAVE_ARGS=("$@")

    # Nord Colors
    readonly NORD11="#BF616A"
    readonly NORD13="#EBCB8B"
    readonly NORD14="#A3BE8C"
    readonly NORD15="#B48EAD"
    readonly NORD6="#ECEFF4"

    # === FUNCTIONS ===

    log_browsing_session() {
        local mission="$1"
        local duration_str="$2"

        local today
        today=$(date '+%Y-%m-%d %A')
        local timestamp
        timestamp=$(date '+[%Y-%m-%d %H:%M]')

        mkdir -p "$(dirname "$LOG_FILE")"

        if [ ! -f "$LOG_FILE" ]; then
            echo "#+TITLE: Browsing Log" > "$LOG_FILE"
            echo "#+STARTUP: overview" >> "$LOG_FILE"
            echo "" >> "$LOG_FILE"
        fi

        if ! grep -q "\* $today" "$LOG_FILE"; then
            echo "" >> "$LOG_FILE"
            echo "* $today" >> "$LOG_FILE"
        fi

        echo "** $mission" >> "$LOG_FILE"
        echo "   Created: $timestamp" >> "$LOG_FILE"
        echo "   Duration: $duration_str" >> "$LOG_FILE"
    }

    launch_browsing_session() {
        local mission="$1"
        local duration_str="$2"
        local duration_seconds="$3"

        # Log the session
        log_browsing_session "$mission" "$duration_str"

        gum style --foreground "$NORD15" --padding "1 2" "✓ launching..."

        # Start timer if duration is set (run in background)
        if [ "$duration_seconds" -gt 0 ]; then
            ( sleep "$duration_seconds" && notify-send -u critical "time up, browsing session ended" ) \
                >/dev/null 2>&1 &
        fi

        # Unique ID for this session
        local unit_timestamp
        unit_timestamp=$(date +%s)

        # Launch Brave in new window (systemd-run returns immediately)
        systemd-run --user --unit="brave-session-$unit_timestamp" \
            "${realBrave}/bin/brave" --new-window "''${BRAVE_ARGS[@]}" \
            >/dev/null 2>&1

        # Launch Emacs side panel with an internal delay so we don't block here
        systemd-run --user --unit="emacs-log-$unit_timestamp" bash -c "
            set -euo pipefail
            sleep 2
            emacs --title 'brave-side-panel' '$LOG_FILE' >/dev/null 2>&1
        " >/dev/null 2>&1

        # Adjust layout in its own unit (with internal delay)
        systemd-run --user --unit="brave-layout-adjuster-$unit_timestamp" bash -c "
            set -euo pipefail
            sleep 3
            hyprctl dispatch focuswindow 'title:^brave-side-panel$' >/dev/null 2>&1 || true
            hyprctl dispatch resizeactive exact 15% 100% >/dev/null 2>&1 || true
        " >/dev/null 2>&1
    }

    handle_browsing_mode() {
        gum style --foreground "$NORD13" --padding "1 2" "authenticate"

        local password
        password=$(gum input \
            --password \
    		--no-show-help \
    		--padding "1 2" \
            --placeholder "pass phrase..." \
            --prompt.foreground "$NORD13" \
            --cursor.foreground "$NORD13" \
            --width 40)

        if [[ "$password" != "$REQUIRED_PASS" ]]; then
            gum style --foreground "$NORD11" --padding "1 2" "✗ incorrect phrase"
            exit 1
        fi

        # Get browsing purpose
        gum style --foreground "$NORD14" --padding "1 2" "purpose"
        local mission
        mission=$(gum input --placeholder "what's the specific goal?" --prompt.foreground "$NORD13")

        if [[ -z "$mission" ]]; then
            local mindless_choice
            mindless_choice=$(gum choose "browse mindlessly" "set a goal" \
                --header "no goal set. proceed anyway?" --cursor.foreground "$NORD13")
            if [[ "$mindless_choice" == "set a goal" ]]; then
                gum style --foreground "$NORD11" --padding "1 2" "✗ cancelled"
                sleep 1
                exit 1
            else
                mission="mindless browsing"
            fi
        fi

        # Get duration
        local duration_str
        duration_str=$(gum choose "5m" "15m" "30m" "1h" "unlimited" \
            --header "Duration?" --cursor.foreground "$NORD13")

        local duration_seconds=0
        case "$duration_str" in
            "5m") duration_seconds=300 ;;
            "15m") duration_seconds=900 ;;
            "30m") duration_seconds=1800 ;;
            "1h") duration_seconds=3600 ;;
            # "unlimited" leaves duration_seconds = 0
        esac

        launch_browsing_session "$mission" "$duration_str" "$duration_seconds"
    }

    handle_project_mode() {
        local choice="$1"

        gum style --foreground "$NORD14" --padding "1 2" "✓ focusing on $choice."

        local hook_script="$HOOKS_DIR/$choice.sh"
        if [ -f "$hook_script" ] && [ -x "$hook_script" ]; then
            "$hook_script"
        else
            gum style --foreground "$NORD6" --padding "1 2" "No hook script found."
            sleep 1
        fi
    }

    # === MAIN ===

    # Check if we are already spawned or if we need a terminal
    if [[ "''${BRAVE_GATEKEEPER_SPAWNED:-}" != "1" ]]; then
        if [[ -z "''${PS1:-}" ]]; then
            exec kitty \
                --class brave-wrapper \
                --single-instance \
                --override window_padding_width=12 \
                --override "modify_font cell_height=120%" \
                -e env BRAVE_GATEKEEPER_SPAWNED=1 "$0" "$@"
        fi
    fi

    # Display menu
    gum style --foreground "$NORD6" --padding "1 2" "Focusing on..."

    # Build project list
    projects=""
    if [ -d "$PROJECTS_DIR" ]; then
        projects=$(find -L "$PROJECTS_DIR" -mindepth 1 -maxdepth 1 -type d -not -name ".*" -printf "%f\n" | sort)
    fi

    options="''${projects}"$'\nbrowse'

    # Get user choice
    choice=$(echo "$options" | gum filter \
        --no-show-help \
        --header "" \
        --indicator.foreground "$NORD13" \
        --match.foreground "$NORD13" \
        --padding "1 2")

    # Route to appropriate handler
    if [[ "$choice" == "browse" ]]; then
        handle_browsing_mode
    else
        handle_project_mode "$choice"
    fi

    exit 0
  '';

  braveDesktop = pkgs.makeDesktopItem {
    name = "brave-gatekeeper";
    desktopName = "Brave Web Browser";
    genericName = "Web Browser";
    exec = "brave %U";
    icon = "brave-browser";
    startupNotify = true;
    terminal = false;
    categories = ["Network" "WebBrowser"];
    mimeTypes = [
      "text/html"
      "text/xml"
      "application/xhtml+xml"
      "x-scheme-handler/http"
      "x-scheme-handler/https"
    ];
  };
in {
  environment.systemPackages = [
    braveGatekeeper
    braveDesktop
  ];
}
