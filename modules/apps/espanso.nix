{
  pkgs,
  ...
}: {
  services.espanso = {
    enable = true;
    package = pkgs.espanso-wayland;

    # Basic Configuration (config/default.yml)
    configs = {
      default = {
        toggle_key = "OFF"; # We use the trigger prefix approach instead of a global toggle key
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

          # Email Example (Placeholder)
          {
            trigger = ":email";
            replace = "my.email@example.com";
          }
        ];
      };
    };
  };
}
