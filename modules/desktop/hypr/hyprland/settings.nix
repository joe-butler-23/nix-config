_: {
  wayland.windowManager.hyprland.settings = {
    monitor = [
      ",preferred,auto,auto"
      "DP-5, preferred, auto, 1"
      "eDP-1, preferred, auto, 1"
      "DP-6, 1920x1080, 0x0, 1"
      "DP-4, 1920x1080, 1920x0, 1"
    ];

    env = [
      "XCURSOR_SIZE,24"
      "HYPRCURSOR_SIZE,24"
      "XDG_CURRENT_DESKTOP,Hyprland"
      "XDG_SESSION_TYPE,wayland"
      "XDG_SESSION_DESKTOP,Hyprland"
      "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
    ];

    general = {
      gaps_in = 0;
      gaps_out = 0;
      border_size = 0;
      resize_on_border = false;
      allow_tearing = false;
      layout = "dwindle";
    };

    decoration = {
      rounding = 0;
      active_opacity = 1.0;
      inactive_opacity = 1.0;
      shadow = {
        enabled = false;
        range = 4;
        render_power = 3;
      };
      blur = {
        enabled = false;
        size = 3;
        passes = 1;
        vibrancy = 0.1696;
      };
    };

    animations = {
      enabled = false;
    };

    dwindle = {
      pseudotile = true;
      preserve_split = true;
    };

    master = {
      new_status = "master";
    };

    misc = {
      force_default_wallpaper = 0;
      disable_hyprland_logo = true;
      vfr = true;
      vrr = 2; # Disable VRR globally
    };

    input = {
      kb_layout = "gb";
      kb_variant = "basic";
      kb_model = "pc105";
      kb_options = "caps:escape";
      kb_rules = "evdev";
      follow_mouse = 1;
      sensitivity = 0;
      touchpad = {
        natural_scroll = false;
      };
    };

    device = {
      name = "epic-mouse-v1";
      sensitivity = -0.5;
    };

    exec-once = [
      "hyprctl dispatch dpms on"
    ];
  };
}
