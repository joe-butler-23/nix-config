{
  pkgs,
  config,
  ...
}: {
  sops.secrets."secrets.yml" = {
    key = "espanso_matches";
    path = "${config.xdg.configHome}/espanso/match/secrets.yml";
  };

  # Manual systemd service - not using services.espanso to avoid config conflicts
  systemd.user.services.espanso = {
    Unit = {
      Description = "Espanso text expander daemon";
      After = ["graphical-session.target" "sops-nix.service"];
      PartOf = ["graphical-session.target"];
      Wants = ["sops-nix.service"];
    };

    Service = {
      ExecStart = "${pkgs.espanso-wayland}/bin/espanso daemon";
      Restart = "on-failure";
    };

    Install.WantedBy = ["graphical-session.target"];
  };
}
