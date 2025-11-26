{
  config,
  lib,
  ...
}: let
  wallpaper = "${config.home.homeDirectory}/nix-config/assets/wallpaper.jpeg";
in {
  # Hyprlock - screen lock
  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        disable_loading_bar = true;
        no_fade_in = true;
        no_fade_out = true;
        hide_cursor = false;
        grace = 0;
        ignore_empty_input = true;
      };

      # Force these paths so they override any conflicting module
      background = {
        monitor = "";
        path = lib.mkForce wallpaper;
        blur_passes = 3;
      };

      image = {
        monitor = "";
        path = lib.mkForce wallpaper;
        size = 75;
        border_size = 2;
        border_color = "#ffffff";
        position = "-50, 50";
        halign = "right";
        valign = "bottom";
      };

      input-field = {
        monitor = "";
        size = "200, 40";
        outline_thickness = 1;
        dots_size = 0.1;
        dots_spacing = 0.35;
        dots_center = true;
        fade_on_empty = false;
        hide_input = false;
        rounding = -1;
        position = "0, -200";
        halign = "center";
        valign = "center";
      };

      label = [
        {
          monitor = "";
          text = "cmd[update:1000] echo \"$(date +\"%A, %B %d\")\"";
          color = "rgba(242, 243, 244, 0.75)";
          font_size = 22;
          font_family = "JetBrains Mono";
          position = "0, 300";
          halign = "center";
          valign = "center";
        }
        {
          monitor = "";
          text = "cmd[update:1000] echo \"$(date +\"%-I:%M\")\"";
          color = "rgba(242, 243, 244, 0.75)";
          font_size = 95;
          font_family = "JetBrains Mono Extrabold";
          position = "0, 200";
          halign = "center";
          valign = "center";
        }
      ];
    };
  };
}
