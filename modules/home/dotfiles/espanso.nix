{
  pkgs,
  ...
}: let
  yaml = pkgs.formats.yaml {};
in {
  # Espanso config files - package installed system-wide
  xdg.configFile = {
    "espanso/config/default.yml".source = yaml.generate "default.yml" {
      backend = "auto";
      injector = "uinput";
      keyboard_layout = {
        layout = "gb";
      };
    };

    "espanso/match/base.yml".source = yaml.generate "base.yml" {
      matches = [
        {
          trigger = ":espanso";
          replace = "Hi there!";
        }
        {
          trigger = ":date";
          replace = "{{mydate}}";
          vars = [
            {
              name = "mydate";
              type = "date";
              params = {
                format = "%m/%d/%Y";
              };
            }
          ];
        }
        {
          trigger = ":shell";
          replace = "{{output}}";
          vars = [
            {
              name = "output";
              type = "shell";
              params = {
                cmd = "echo 'Hello from your shell'";
              };
            }
          ];
        }
        {
          trigger = ":email";
          replace = "joeb.92@hotmail.co.uk";
        }
        {
          trigger = ":gmail";
          replace = "your-email@gmail.com";
        }
      ];
    };
  };
}
