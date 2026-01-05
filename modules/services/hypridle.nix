{pkgs, ...}: {
  # Hypridle idle management daemon (user service)
  # Config managed by chezmoi
  systemd.user.services.hypridle = {
    description = "Hyprland's idle daemon";
    documentation = ["https://wiki.hyprland.org/Hypr-Ecosystem/hypridle"];
    after = ["graphical-session.target"];
    partOf = ["graphical-session.target"];
    wantedBy = ["graphical-session.target"];

    serviceConfig = {
      ExecStart = "${pkgs.hypridle}/bin/hypridle";
      Restart = "on-failure";
      RestartSec = "5s";
    };

    # Required for hyprlock, hyprctl, brightnessctl commands
    path = ["/run/current-system/sw"];
  };
}
