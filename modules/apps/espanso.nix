{
  lib,
  pkgs,
  config,
  ...
}: {
  sops.secrets."secrets.yml" = {
    key = "espanso_matches";
    path = "${config.xdg.configHome}/espanso/match/secrets.yml";
  };

  services.espanso = {
    enable = true;
    package = pkgs.espanso-wayland;

    # Config and matches now managed by chezmoi instead of Nix
    # configs.default and matches.base removed to avoid conflicts
  };

  systemd.user.services.espanso = {
    Unit = {
      After = ["graphical-session.target" "sops-nix.service"];
      PartOf = ["graphical-session.target"];
      Wants = ["sops-nix.service"];
    };

    Install.WantedBy = lib.mkForce ["graphical-session.target"];
  };
}
