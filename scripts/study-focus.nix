{ pkgs }:

pkgs.writeShellScriptBin "study-focus" ''
  set -euo pipefail

  # Study Focus Script
  # Simple study mode that blocks applications and sends notifications

  STATE_FILE="/tmp/study-focus-state"

  # Configuration embedded in script
  BLOCKED_APPS="firefox google-chrome brave"
  ACTIVATE_MSG="Study mode activated"
  DEACTIVATE_MSG="Study mode deactivated"

  # Send notification
  send_notification() {
      local message="$1"
      
      # Try different notification methods
      if command -v ${pkgs.libnotify}/bin/notify-send >/dev/null 2>&1; then
          ${pkgs.libnotify}/bin/notify-send "Study Focus" "$message" -u normal
      elif command -v ${pkgs.dunst}/bin/dunstify >/dev/null 2>&1; then
          ${pkgs.dunst}/bin/dunstify "Study Focus" "$message"
      else
          echo "Study Focus: $message"
      fi
  }

  # Block applications
  block_apps() {
      for app in $BLOCKED_APPS; do
          # Kill running instances
          ${pkgs.procps}/bin/pkill -f "$app" 2>/dev/null && echo "Blocked: $app"
      done
  }

  # Monitor function for systemd service
  monitor_apps() {
      while [[ -f "$STATE_FILE" ]]; do
          for app in $BLOCKED_APPS; do
              if ${pkgs.procps}/bin/pgrep -f "$app" >/dev/null 2>&1; then
                  echo "Blocking new instance of: $app"
                  ${pkgs.procps}/bin/pkill -f "$app" 2>/dev/null
                  send_notification "Blocked: $app - Study mode is active!"
              fi
          done
          sleep 2
      done
  }

  # Check if systemd user services are available
  has_systemd() {
      command -v ${pkgs.systemd}/bin/systemctl >/dev/null 2>&1 && ${pkgs.systemd}/bin/systemctl --user list-units >/dev/null 2>&1
  }

  # Create systemd services
  create_systemd_services() {
      local user_config_dir="$HOME/.config/systemd/user"
      ${pkgs.coreutils}/bin/mkdir -p "$user_config_dir"
      
      # Create monitor service
      cat > "$user_config_dir/study-focus-monitor.service" << 'EOF'
[Unit]
Description=Study Focus Application Monitor
WantedBy=study-focus-block.target

[Service]
Type=simple
ExecStart=/home/joebutler/.nix-profile/bin/study-focus monitor
Restart=always
RestartSec=1
StartLimitIntervalSec=60
StartLimitBurst=5

[Install]
WantedBy=study-focus-block.target
EOF

      # Create target
      cat > "$user_config_dir/study-focus-block.target" << 'EOF'
[Unit]
Description=Study Focus Block Target
StopWhenUnneeded=yes
EOF

      # Create individual blocker services
      for app in firefox google-chrome brave; do
          cat > "$user_config_dir/study-focus-block-$app.service" << EOF
[Unit]
Description=Block $app during study focus
PartOf=study-focus-block.target

[Service]
Type=oneshot
ExecStart=/bin/true

[Install]
WantedBy=study-focus-block.target
EOF
      done
      
      ${pkgs.systemd}/bin/systemctl --user daemon-reload >/dev/null 2>&1
  }

  # Start systemd blocking
  start_systemd_block() {
      if has_systemd; then
          create_systemd_services
          ${pkgs.systemd}/bin/systemctl --user start study-focus-monitor.service >/dev/null 2>&1
          ${pkgs.systemd}/bin/systemctl --user enable study-focus-monitor.service >/dev/null 2>&1
      fi
  }

  # Stop systemd blocking
  stop_systemd_block() {
      if has_systemd; then
          ${pkgs.systemd}/bin/systemctl --user stop study-focus-monitor.service >/dev/null 2>&1
          ${pkgs.systemd}/bin/systemctl --user disable study-focus-monitor.service >/dev/null 2>&1
      fi
  }

  # Check if study mode is active
  is_active() {
      [[ -f "$STATE_FILE" ]] && [[ "$(${pkgs.coreutils}/bin/cat "$STATE_FILE")" == "active" ]]
  }

  # Activate study mode
  activate_study_mode() {
      if is_active; then
          echo "Study focus is already active"
          exit 0
      fi
      
      echo "Activating study focus..."
      block_apps
      echo "active" > "$STATE_FILE"
      start_systemd_block
      send_notification "$ACTIVATE_MSG"
      echo "Study focus activated"
  }

  # Deactivate study mode
  deactivate_study_mode() {
      if ! is_active; then
          echo "Study focus is not active"
          exit 0
      fi
      
      echo "Deactivating study focus..."
      stop_systemd_block
      ${pkgs.coreutils}/bin/rm -f "$STATE_FILE"
      send_notification "$DEACTIVATE_MSG"
      echo "Study focus deactivated"
  }

  # Show status
  show_status() {
      if is_active; then
          echo "Study focus is ACTIVE"
          echo "Blocked applications:"
          for app in $BLOCKED_APPS; do
              echo "  - $app"
          done
      else
          echo "Study focus is INACTIVE"
      fi
  }

  # Show help
  show_help() {
      cat << EOF
  Study Focus - Simple focus mode by blocking distracting applications

  Usage: study-focus [COMMAND]

  Commands:
      activate      Activate study focus (default)
      deactivate    Deactivate study focus
      toggle        Toggle study focus on/off
      status        Show current status
      help          Show this help message

  Configuration:
      Edit the script directly to customize blocked applications and messages

  Examples:
      study-focus activate      # Activate study focus
      study-focus deactivate    # Deactivate study focus
      study-focus toggle        # Toggle study focus
      study-focus               # Activate study focus (default)
EOF
  }

  # Main function
  main() {
      case "''${1:-activate}" in
          "activate"|"on"|"start")
              activate_study_mode
              ;;
          "deactivate"|"off"|"stop")
              deactivate_study_mode
              ;;
          "toggle")
              if is_active; then
                  deactivate_study_mode
              else
                  activate_study_mode
              fi
              ;;
          "status")
              show_status
              ;;
          "monitor")
              monitor_apps
              ;;
          "help"|"-h"|"--help")
              show_help
              ;;
          *)
              echo "Unknown command: $1"
              show_help
              exit 1
              ;;
      esac
  }

  main "$@"
''
