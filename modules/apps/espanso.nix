{
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

    # Basic Configuration (config/default.yml)
    configs = {
      default = {
        toggle_key = "OFF";
        backend = "Clipboard";
      };
    };

    # Expansions (match/base.yml)
    matches = {
      base = {
        matches = [
          # Date Example
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
        ];
      };
    };
  };
}
