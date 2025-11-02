{
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland = {
      enable = true;
    };
    settings = {
      monitor = [
        ",preferred,auto,auto"
        "DP-5, preferred, auto, 1, vrr, 1"
        "eDP-1, preferred, auto, 1, vrr, 1"
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
        border_size = 1;
        col.active_border = "rgba(A7A7A780)";
        col.inactive_border = "rgba(A7A7A780)";
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
          color = "rgba(1a1a1aee)";
        };
        blur = {
          enabled = false;
          size = 3;
          passes = 1;
          vibrancy = 0.1696;
        };
      };

      animations = {
        enabled = "yes";
        bezier = [
          "easeOutQuint,0.23,1,0.32,1"
          "easeInOutCubic,0.65,0.05,0.36,1"
          "linear,0,0,1,1"
          "almostLinear,0.5,0.5,0.75,1.0"
          "quick,0.15,0,0.1,1"
        ];
        animation = [
          "global, 0, 10, default"
          "border, 0, 5.39, easeOutQuint"
          "windows, 0, 4.79, easeOutQuint"
          "windowsIn, 0, 4.1, easeOutQuint, popin 87%"
          "windowsOut, 0, 1, linear, fade"
          "fadeOut,   0, 1, linear"
          "fadeIn, 0, 1.73, almostLinear"
          "fade, 0, 3.03, quick"
          "layers, 0, 3.81, easeOutQuint"
          "layersIn, 0, 4, easeOutQuint, fade"
          "layersOut, 0, 1.5, linear, fade"
          "fadeLayersIn, 0, 1.79, almostLinear"
          "fadeLayersOut, 0, 1.39, almostLinear"
          "workspaces, 0, 1.94, almostLinear, fade"
          "workspacesIn, 0, 1.21, almostLinear, fade"
          "workspacesOut, 0, 1.94, almostLinear, fade"
        ];
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
    };

    extraConfig = ''
      $mainMod = SUPER
      $terminal = foot
      $menu = rofi -show drun -matching regex -no-tokenize -drun-match-fields 'name'  -drun-display-format '{name}' -run-command "app2unit -- {cmd}"
      $browser = app2unit brave
      $fileManager = app2unit foot -a yazi -D ~ sh -lc 'TMPFILE=$(mktemp); yazi --chooser-file="$TMPFILE"; if [ -s "$TMPFILE" ]; then xdg-open "$(cat "$TMPFILE")"; fi; rm -f "$TMPFILE"'

      bind = $mainMod, Return, exec, $terminal
      bind = $mainMod, C, killactive,
      bind = $mainMod, E, exec, $fileManager
      bind = $mainMod, V, togglefloating,
      bindr = $mainMod, SUPER_L, exec, $menu
      bind = $mainMod, P, pseudo, # dwindle
      bind = $mainMod, U, togglesplit, # dwindle
      bind = $mainMod, B, exec, $browser
      bind = $mainMod, Y, exec, app2unit hyprshot -m region --clipboard-only
      bind = $mainMod, SPACE, exec, /home/joebutler/.cargo/bin/wlr-which-key
      bind = $mainMod, a, exec, app2unit footclient
      bind = $mainMod, F, exec, app2unit footclient -a filepicker -D ~ sh -lc "$HOME/bin/fzf_file_launcher.sh"
      bind = $mainMod, R, exec, ~/bin/refile-mousepad.sh
      bind = $mainMod, F3, exec, hyprctl keyword monitor eDP-1,preferred,0x0,1 && hyprctl keyword monitor DP-5,disable && hyprctl dispatch moveworkspacetomonitor all eDP-1
      bind = $mainMod, m, exec, ~/.config/hypr/toggle_eDP1.sh
      bind = $mainMod, n, exec, hyprctl keyword monitor eDP-1,enable

      bind = $mainMod, h, movefocus, l
      bind = $mainMod, l, movefocus, r
      bind = $mainMod, k, movefocus, u
      bind = $mainMod, j, movefocus, d

      bind = $mainMod, 1, workspace, 1
      bind = $mainMod, 2, workspace, 2
      bind = $mainMod, 3, workspace, 3
      bind = $mainMod, 4, workspace, 4
      bind = $mainMod, 5, workspace, 5
      bind = $mainMod, 6, workspace, 6
      bind = $mainMod, 7, workspace, 7
      bind = $mainMod, 8, workspace, 8
      bind = $mainMod, 9, workspace, 9
      bind = $mainMod, 0, workspace, 10
      bind = $mainMod SHIFT, h, workspace, e-1
      bind = $mainMod SHIFT, l, workspace, e+1

      bind = $mainMod SHIFT, 1, movetoworkspace, 1
      bind = $mainMod SHIFT, 2, movetoworkspace, 2
      bind = $mainMod SHIFT, 3, movetoworkspace, 3
      bind = $mainMod SHIFT, 4, movetoworkspace, 4
      bind = $mainMod SHIFT, 5, movetoworkspace, 5
      bind = $mainMod SHIFT, 6, movetoworkspace, 6
      bind = $mainMod SHIFT, 7, movetoworkspace, 7
      bind = $mainMod SHIFT, 8, movetoworkspace, 8
      bind = $mainMod SHIFT, 9, movetoworkspace, 9
      bind = $mainMod SHIFT, 0, movetoworkspace, 10

      bind = $mainMod, S, togglespecialworkspace, magic
      bind = $mainMod SHIFT, S, movetoworkspace, special:magic

      bind = $mainMod, mouse_down, workspace, e+1
      bind = $mainMod, mouse_up, workspace, e-1
      bind = , mouse:275, workspace, e-1
      bind = , mouse:276, workspace, e+1

      bindm = $mainMod, mouse:272, movewindow
      bindm = $mainMod, mouse:273, resizewindow

      bindel = ,XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+
      bindel = ,XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
      bindel = ,XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
      bindel = ,XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle
      bindel = ,XF86MonBrightnessUp, exec, brightnessctl s 10%+
      bindel = ,XF86MonBrightnessDown, exec, brightnessctl s 10%-

      bind = $mainMod SHIFT, SPACE, exec, app2unit $HOME/bin/obsidian_capture.sh

      windowrulev2 = suppressevent maximize, class:.*
      windowrulev2 = nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0
      windowrulev2 = float, class:filepicker
      windowrulev2 = center, class:filepicker
      windowrulev2 = size 70% 40%, class:filepicker
      windowrulev2 = stayfocused, class:filepicker
      windowrulev2 = float, class:dirfinder
      windowrulev2 = center, class:dirfinder
      windowrulev2 = size 600 120, class:dirfinder
      windowrulev2 = opacity 0.9, class:dirfinder
      windowrulev2 = float, class:clipse
      windowrulev2 = center, class:clipse
      windowrulev2 = size 70% 40%, class:clipse
      windowrulev2 = stayfocused, class:clipse
      windowrulev2 = float, persistentsize, class:blueman-manager
    '';
  };
}
