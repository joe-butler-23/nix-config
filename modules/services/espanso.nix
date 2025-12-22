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
      # Copy secret to user runtime dir (RAM) and symlink it. %t resolves to /run/user/<uid>
      ExecStartPre = pkgs.writeShellScript "espanso-pre" ''
        ${pkgs.coreutils}/bin/mkdir -p %t/espanso
        ${pkgs.coreutils}/bin/cp -f /run/secrets/espanso_matches %t/espanso/secrets.yml
        ${pkgs.coreutils}/bin/chmod 600 %t/espanso/secrets.yml
        ${pkgs.coreutils}/bin/mkdir -p /home/${user}/.config/espanso/match
        ${pkgs.coreutils}/bin/ln -sf %t/espanso/secrets.yml /home/${user}/.config/espanso/match/secrets.yml
      '';
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = "PATH=${pkgs.wl-clipboard}/bin:${pkgs.libnotify}/bin:$PATH";
    };
  };

  # SOPS secret for espanso
  sops.secrets.espanso_matches = {
    owner = user;
    # No path specified -> defaults to /run/secrets/espanso_matches
  };
}
