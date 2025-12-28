{
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
      ExecStart = "${pkgs.espanso-wayland}/bin/espanso daemon";
      # Copy secret to user runtime dir (RAM) and symlink it.
      # Fix: Use shell variables instead of systemd specifiers inside the script body.
      ExecStartPre = pkgs.writeShellScript "espanso-pre" ''
        set -e
        RUNTIME_DIR="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

        ${pkgs.coreutils}/bin/mkdir -p "$RUNTIME_DIR/espanso"

        # Wait up to 10s for the secret to be decrypted by sops-nix
        for i in {1..10}; do
          if [ -f /run/secrets/espanso_matches ]; then
            break
          fi
          ${pkgs.coreutils}/bin/sleep 1
        done

        if [ ! -f /run/secrets/espanso_matches ]; then
          echo "Error: /run/secrets/espanso_matches not found. sops-nix might have failed."
          exit 1
        fi

        ${pkgs.coreutils}/bin/cp -f /run/secrets/espanso_matches "$RUNTIME_DIR/espanso/secrets.yml"
        ${pkgs.coreutils}/bin/chmod 600 "$RUNTIME_DIR/espanso/secrets.yml"

        # Ensure user config directory exists
        ${pkgs.coreutils}/bin/mkdir -p "/home/${user}/.config/espanso/match"

        # Create symlink using absolute paths to avoid relative path confusion
        ${pkgs.coreutils}/bin/ln -sf "$RUNTIME_DIR/espanso/secrets.yml" "/home/${user}/.config/espanso/match/secrets.yml"
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
    owner = user;
    # No path specified -> defaults to /run/secrets/espanso_matches
  };
}
