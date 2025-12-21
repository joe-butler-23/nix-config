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
      Restart = "on-failure";
    };
  };

  # SOPS secret for espanso
  sops.secrets."secrets.yml" = {
    owner = user;
    path = "/home/${user}/.config/espanso/match/secrets.yml";
  };
}
