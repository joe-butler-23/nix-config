_: {
  programs.waybar = {
    enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "bottom";
        height = 12;
        mode = "dock";
        reload_style_on_change = true;
        gtk-layer-shell = true;

        modules-left = [
          "hyprland/workspaces"
        ];

        modules-center = [
          "hyprland/window"
        ];

        modules-right = [
          "idle_inhibitor"
          "pulseaudio"
          "temperature"
          "backlight"
          "battery"
          "clock"
          "tray"
          "custom/power"
        ];

        "hyprland/workspaces" = {
          disable-scroll = true;
          all-outputs = true;
          warp-on-scroll = false;
          format = "{name}: {icon}";
          format-icons = {
            urgent = "";
            active = "";
            default = "";
          };
        };

        tray = {
          icon-size = 16;
          spacing = 10;
        };

        clock = {
          format = "{:%H:%M} 🕐 ";
          format-alt = "{:%A, %B %d, %Y (%R)} 📅 ";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
            mode = "month";
            mode-mon-col = 3;
            weeks-pos = "right";
            on-scroll = 1;
            format = {
              months = "<span color='#ffead3'><b>{}</b></span>";
              days = "<span color='#ecc6d9'><b>{}</b></span>";
              weeks = "<span color='#99ffdd'><b>W{}</b></span>";
              weekdays = "<span color='#ffcc66'><b>{}</b></span>";
              today = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
          actions = {
            on-click-right = "mode";
            on-scroll-up = "shift_up";
            on-scroll-down = "shift_down";
          };
        };

        cpu = {
          format = "{usage}% ";
          tooltip = false;
        };

        memory = {
          format = "{}% ";
        };

        temperature = {
          critical-threshold = 80;
          format = "{temperatureC}°C {icon}";
          format-icons = ["" "" ""];
        };

        backlight = {
          format = "{percent}% {icon}";
          format-icons = ["" "" "" "" "" "" "" "" ""];
        };

        battery = {
          states = {
            warning = 30;
            critical = 15;
          };
          format = "{capacity}% {icon}";
          format-full = "{capacity}% {icon}";
          format-charging = "{capacity}% ";
          format-plugged = "{capacity}% ";
          format-alt = "{time} {icon}";
          format-icons = ["" "" "" "" ""];
        };

        power-profiles-daemon = {
          format = "{icon}";
          tooltip-format = "Power profile: {profile}\nDriver: {driver}";
          tooltip = true;
          format-icons = {
            default = "";
            performance = "";
            balanced = "";
            power-saver = "";
          };
        };

        network = {
          format-wifi = "{essid} ({signalStrength}%) ";
          format-ethernet = "{ipaddr}/{cidr} ";
          tooltip-format = "{ifname} via {gwaddr} ";
          format-linked = "{ifname} (No IP) ";
          format-disconnected = "Disconnected ⚠";
          format-alt = "{ifname}: {ipaddr}/{cidr}";
        };

        pulseaudio = {
          format = "{volume}% {icon} {format_source}";
          format-bluetooth = "{volume}% {icon} {format_source}";
          format-bluetooth-muted = " {icon} {format_source}";
          format-muted = " {format_source}";
          format-source = "{volume}% ";
          format-source-muted = "";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = ["" "" ""];
          };
          on-click = "pavucontrol";
        };

        "custom/media" = {
          format = "{icon} {text}";
          return-type = "json";
          max-length = 40;
          format-icons = {
            spotify = "";
            default = "🎜";
          };
          escape = true;
          exec = "$HOME/.config/waybar/mediaplayer.py 2> /dev/null";
        };

        "custom/power" = {
          format = "⏻ ";
          tooltip = false;
          on-click = "wlogout";
        };
      };
    };

    # Minimal style override to preserve font size, add dark background and opacity
    style = ''
      * {
        font-size: 12px;
      }
      
      window#waybar {
        background-color: rgba(0, 0, 0, 0.9);
        color: #ffffff;
        opacity: 0.8;
      }
    '';

    # Full custom styling commented out - only font-size override above
    # Uncomment and modify if you want to override Stylix colors
    # Original style = ''
    #   /* Global settings */
    #   * {
    #     border: none;
    #     border-radius: 4px;
    #     font-family: "JetBrains Mono", monospace;
    #     font-size: 12px;
    #     min-height: 0;
    #   }
    #
    #   /* Waybar window style */
    #   window#waybar {
    #     background-color: rgba(0, 0, 0, 0.9);
    #     color: #ffffff;
    #   }
    #
    #   /* Workspaces */
    #   #workspaces button {
    #     color: #ffffff;
    #     box-shadow: inset 0 -3px transparent;
    #   }
    #
    #   #workspaces button:hover {
    #     background: rgba(0, 0, 0, 0.9);
    #     box-shadow: inset 0 -3px #ffffff;
    #   }
    #
    #   #workspaces button.focused {
    #     background-color: #2c3e50;
    #   }
    #
    #   #workspaces button.urgent {
    #     background-color: #f53c3c;
    #   }
    #
    #   /* Mode widget */
    #   #mode {
    #     background-color: #2c3e50;
    #   }
    #
    #   /* Module common styles (padding, margin, and a right border for separation) */
    #   #clock,
    #   #battery,
    #   #power-profiles-daemon,
    #   #cpu,
    #   #memory,
    #   #temperature,
    #   #backlight,
    #   #pulseaudio,
    #   #custom-media,
    #   #tray,
    #   #idle_inhibitor,
    #   #custom-power {
    #     padding: 0 5px;
    #     margin: 3px 3px;
    #     border-right: 1px solid #444;
    #   }
    #
    #   /* Remove right border for the last widget in a group */
    #   .modules-right > widget:last-child,
    #   .modules-left > widget:first-child {
    #     border-right: none;
    #   }
    #
    #   /* Clock module */
    #   #clock {
    #     background-color: #2c3e50;
    #     color: #ffffff;
    #   }
    #
    #   /* Battery module */
    #   #battery {
    #     background-color: #4CAF50;
    #     color: #000000;
    #     min-width: 50px;
    #   }
    #
    #   #battery.charging {
    #     background-color: #4CAF50;
    #     color: #000000;
    #     min-width: 50px;
    #   }
    #
    #   #battery.critical:not(.charging) {
    #     background-color: #f53c3c;
    #     color: #000000;
    #     min-width: 50px;
    #     animation: blink 0.5s linear infinite alternate;
    #   }
    #
    #   #power-profiles-daemon {
    #     background-color: #2980b9;
    #     color: #ffffff;
    #     border-right: 1px solid #444;
    #     min-width: 20px;
    #   }
    #
    #   /* Blink animation for critical battery */
    #   @keyframes blink {
    #     to {
    #       background-color: #ffffff;
    #       color: rgba(0, 0, 0, 0.9);
    #     }
    #   }
    #
    #   /* CPU module */
    #   #cpu {
    #     background-color: #9b59b6;
    #     color: #ffffff;
    #   }
    #
    #   /* Memory module */
    #   #memory {
    #     background-color: #2c3e50;
    #     color: #ffffff;
    #   }
    #
    #   /* Backlight module */
    #   #backlight {
    #     background-color: #2c3e50;
    #     color: #ffffff;
    #   }
    #
    #   /* Pulseaudio (audio) module */
    #   #pulseaudio {
    #     background-color: #f0932b;
    #     color: #000000;
    #   }
    #
    #   #pulseaudio.muted {
    #     background-color: #f0932b;
    #     color: #000000;
    #   }
    #
    #   /* Temperature module */
    #   #temperature {
    #     background-color: #f0932b;
    #     color: #ffffff;
    #   }
    #
    #   #temperature.critical {
    #     background-color: #f53c3c;
    #   }
    #
    #   /* Custom Media module */
    #   #custom-media {
    #     background-color: #4CAF50;
    #     color: #2c3e50;
    #     min-width: 100px;
    #   }
    #
    #   #custom-media.custom-spotify {
    #     background-color: #4CAF50;
    #   }
    #
    #   #custom-media.custom-vlc {
    #     background-color: #ffa000;
    #   }
    #
    #   /* Tray module */
    #   #tray {
    #     background-color: #2980b9;
    #     color: #000000;
    #   }
    #
    #   /* Idle Inhibitor module */
    #   #idle_inhibitor {
    #     background-color: #2d3436;
    #     color: #ffffff;
    #   }
    #
    #   #idle_inhibitor.activated {
    #     background-color: #ffffff;
    #     color: #2c3e50;
    #   }
    #
    #   /* Custom Power module */
    #   #custom-power {
    #     background-color: #ffffff;
    #     color: #2c3e50;
    #   }
    #
    #   /* Additional focus styles */
    #   label:focus {
    #     background-color: rgba(0, 0, 0, 0.9);
    #   }
    # '';
  };
}
