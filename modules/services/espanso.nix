{
  config,
  pkgs,
  user,
  ...
}: {
  # Espanso text expander daemon (user service)
  systemd.user.services.espanso = {
    description = "Espanso text expander daemon";
    after = ["graphical-session.target" "sops-nix.service"];
    partOf = ["graphical-session.target"];
    wants = ["sops-nix.service"];
    wantedBy = ["graphical-session.target"];

    serviceConfig = {
      ExecStart = pkgs.writeShellScript "espanso-start" ''
        set -e
        RUNTIME_DIR="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

        # Wait up to 10s for a Wayland socket so espanso doesn't race the compositor
        for i in {1..20}; do
          for sock in "$RUNTIME_DIR"/wayland-*; do
            if [ -S "$sock" ]; then
              export WAYLAND_DISPLAY="''${sock##*/}"
              break 2
            fi
          done
          ${pkgs.coreutils}/bin/sleep 0.5
        done

        if [ -z "''${WAYLAND_DISPLAY:-}" ]; then
          echo "Error: Wayland socket not found in $RUNTIME_DIR"
          exit 1
        fi

        exec ${pkgs.espanso-wayland}/bin/espanso daemon
      '';
      # Symlink the rendered secret into the espanso match directory.
      ExecStartPre = pkgs.writeShellScript "espanso-pre" ''
        set -e
        SECRET_PATH="${config.sops.templates."espanso_matches.yml".path}"

        # Wait up to 10s for the secret to be decrypted by sops-nix
        for i in {1..10}; do
          if [ -f "$SECRET_PATH" ]; then
            break
          fi
          ${pkgs.coreutils}/bin/sleep 1
        done

        if [ ! -f "$SECRET_PATH" ]; then
          echo "Error: $SECRET_PATH not found. sops-nix might have failed."
          exit 1
        fi

        # Ensure user config directory exists
        ${pkgs.coreutils}/bin/mkdir -p "/home/${user}/.config/espanso/match"

        # Create symlink using absolute paths to avoid relative path confusion
        ${pkgs.coreutils}/bin/ln -sf "$SECRET_PATH" "/home/${user}/.config/espanso/match/secrets.yml"
      '';
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = [
        "PATH=${pkgs.wl-clipboard}/bin:${pkgs.kdotool}/bin:${pkgs.libnotify}/bin:${pkgs.espanso-wayland}/bin:$PATH"
      ];
    };
  };

  # SOPS secret for espanso
  sops.secrets.espanso_matches = {
    mode = "0400";
    owner = user;
    inherit (config.users.users.${user}) group;
  };

  # Render a match file with an extension for espanso's loader.
  sops.templates."espanso_matches.yml" = {
    content = config.sops.placeholder.espanso_matches;
    mode = "0400";
    owner = user;
    inherit (config.users.users.${user}) group;
  };
}
