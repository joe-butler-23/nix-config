{pkgs, ...}: let
  # Update script that runs all overlay update scripts
  updateOverlaysScript = pkgs.writeShellScript "update-overlays" ''
    set -euo pipefail

    CONFIG_DIR="/home/joebutler/nix-config"

    if [ ! -d "$CONFIG_DIR" ]; then
      echo "Config directory $CONFIG_DIR not found!"
      exit 1
    fi

    cd "$CONFIG_DIR"

    echo "$(date): Starting overlay updates..."

    # Run each update script
    UPDATED=0

    if [ -n "${pkgs.opencode.passthru.updateScript or ""}" ]; then
      echo "Updating opencode..."
      if ${pkgs.opencode.passthru.updateScript or ""}; then
        UPDATED=1
      fi
    fi

    if [ -n "${pkgs.gemini.passthru.updateScript or ""}" ]; then
      echo "Updating gemini-cli..."
      if ${pkgs.gemini.passthru.updateScript or ""}; then
        UPDATED=1
      fi
    fi

    if [ -n "${pkgs.claude.passthru.updateScript or ""}" ]; then
      echo "Updating claude-code..."
      if ${pkgs.claude.passthru.updateScript or ""}; then
        UPDATED=1
      fi
    fi

    if [ -n "${pkgs.codex.passthru.updateScript or ""}" ]; then
      echo "Updating codex..."
      if ${pkgs.codex.passthru.updateScript or ""}; then
        UPDATED=1
      fi
    fi

    if [ -n "${pkgs.zed-editor.passthru.updateScript or ""}" ]; then
      echo "Updating zed-editor..."
      if ${pkgs.zed-editor.passthru.updateScript or ""}; then
        UPDATED=1
      fi
    fi

    # Check if overlays file changed
    if git diff --quiet modules/overlays/default.nix; then
      echo "No overlay updates available."
      exit 0
    fi

    echo "Overlays updated. Committing changes..."
    git add modules/overlays/default.nix
    git commit -m "chore: update overlay packages [automated]"
    git push

    echo "$(date): Overlay updates completed and pushed."
  '';
in {
  systemd.services.overlay-updates = {
    description = "Update overlay packages";
    after = ["network-online.target"];
    wants = ["network-online.target"];
    path = with pkgs; [curl jq git openssh];
    environment = {
      HOME = "/home/joebutler";
      GIT_SSH_COMMAND = "ssh -o StrictHostKeyChecking=no";
    };
    serviceConfig = {
      Type = "oneshot";
      User = "joebutler";
      ExecStart = updateOverlaysScript;
      WorkingDirectory = "/home/joebutler/nix-config";
      X-RestartIfChanged = false;
    };
  };

  systemd.timers.overlay-updates = {
    description = "Timer for overlay updates";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
      Unit = "overlay-updates.service";
    };
  };
}
