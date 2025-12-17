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

    # config/default.yml
    configs.default = {
      toggle_key = "OFF";
      show_notifications = false;
      auto_restart = true;
      backend = "auto";

      app_specific_configs = [
        {
          filter_class = "kitty";
          config = {
            paste_shortcut = "CTRL+SHIFT+V";
          };
        }
      ];

      keyboard_layout = {
        layout = "gb";
      };
    };

    # match/base.yml
    matches.base = {
      matches = [
        {
          trigger = ":now";
          replace = "{{mytime}}";
          vars = [
            {
              name = "mytime";
              type = "date";
              params = {
                format = "%H:%M";
              };
            }
          ];
        }

        {
          trigger = ":date";
          replace = "{{mydate}}";
          vars = [
            {
              name = "mydate";
              type = "date";
              params = {
                format = "%Y-%m-%d";
              };
            }
          ];
        }

        {
          trigger = ":datetime";
          replace = "{{mydatetime}}";
          vars = [
            {
              name = "mydatetime";
              type = "date";
              params = {
                format = "%Y-%m-%d %H:%M";
              };
            }
          ];
        }
        {
          trigger = ":skill";
          replace = "Your first step is now to confirm which of the available skills you think are required for this task and repeat them back to me. Then stop and wait for my confirmation.";
        }
        {
          trigger = ":skon";
          replace = "Great. Please now use openskills to load those skills as your next priority. And after that is done, proceed with the following:";
        }
      ];
    };
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
