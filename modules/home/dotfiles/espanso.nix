{
  pkgs,
  ...
}: {
  services.espanso = {
    enable = true;
    package = pkgs.espanso-wayland;

    configs = {
      default = {
        backend = "auto";
        injector = "uinput";
        keyboard_layout = {
          layout = "gb";
        };
      };
    };

    matches = {
      base = {
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
  };
}
