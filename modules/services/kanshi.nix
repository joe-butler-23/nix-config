{pkgs, ...}: {
  # Kanshi monitor daemon (user service)
  # Config managed by chezmoi at ~/.config/kanshi/config
  systemd.user.services.kanshi = {
    description = "Kanshi output autoconfig";
    partOf = ["graphical-session.target"];
    wantedBy = ["graphical-session.target"];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.kanshi}/bin/kanshi";
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };
}
