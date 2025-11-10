{
  config,
  lib,
  ...
}: let
  # One absolute path for everything (avoids $HOME expansion issues)
  wallpaper = "${config.home.homeDirectory}/nix-config/wallpaper.jpeg";
in {
  # Hyprpaper - wallpaper management
  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = {
        on_shutdown = ["hyprctl dispatch dpms on"];
      };
      preload = [wallpaper];
      wallpaper = [",${wallpaper}"];
    };
  };

  # Hypridle - idle management daemon
  services.hypridle = {
    enable = true;
    settings = {
      general = {
        lock_cmd = "pidof hyprlock || hyprlock";
        before_sleep_cmd = "loginctl lock-session";
        after_sleep_cmd = "hyprctl dispatch dpms on";
        ignore_dbus_inhibit = false;
      };

      listener = [
        {
          timeout = 150;
          on-timeout = "brightnessctl -s set 10";
          on-resume = "brightnessctl -r";
        }
        {
          timeout = 150;
          on-timeout = "brightnessctl -sd dell::kbd_backlight set 0";
          on-resume = "brightnessctl -rd dell::kbd_backlight";
        }
        {
          timeout = 180;
          on-timeout = "loginctl lock-session";
        }
        {
          timeout = 330;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        }
        {
          timeout = 1800;
          on-timeout = "systemctl suspend";
        }
      ];
    };
  };

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
