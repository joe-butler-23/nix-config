{ config, ... }:
{
  networking.hostName = "laptop-nix";

  assertions = [{
    assertion = config.networking.hostName == "laptop-nix";
    message = "Refusing to build: this module is only for host 'laptop-nix'.";
  }];

  # Laptop-only Home-Manager config
  home-manager.users.joebutler = {
    services.kanshi.enable = true;

    xdg.configFile."kanshi/config".text = ''
profile undocked {
  output eDP-1 enable position 0,0 mode 1920x1080
  exec hyprctl reload
}

profile docked {
  output "Dell Inc. DELL S2721HSX 1991Q83" enable position 1920,0 mode 1920x1080
  output eDP-1 disable
  exec hyprctl reload
}
    '';
  };
}
