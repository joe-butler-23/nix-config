{config, ...}: let
  dot = "${config.home.homeDirectory}/.dotfiles";
in {
  programs.yazi = {
    enable = true;

    # Main yazi configuration
    settings = {
      opener = {
        edit = [
          {
            run = ''[ -f "$1" ] && nvim "$1"'';
            desc = "Edit with Neovim";
            for = "unix";
          }
          {
            run = ''[ -f "$1" ] && echo "$1" > "''${CHOOSER}"'';
            desc = "Pick file and quit";
            for = "unix";
          }
        ];

        open = [
          {
            run = ''xdg-open "$@"'';
            desc = "Open with system default";
            for = "unix";
            block = false;
            orphan = true;
          }
        ];
      };
    };

    # Keymap configuration
    keymap = {
      mgr.prepend_keymap = [
        {
          on = "<Enter>";
          run = ["open" "quit"];
          desc = "Open file with default app and quit";
        }
      ];

      mgr.append_keymap = [
        {
          on = "o";
          run = "open";
          desc = "Open file with default app and stay";
        }
      ];
    };

    # Theme configuration
    theme = {
      flavor = {
        dark = "kanagawa-dragon";
      };
    };
  };

  # Keep flavors and plugins as individual symlinks (external content)
  xdg.configFile."yazi/flavors/ashen.yazi".source = config.lib.file.mkOutOfStoreSymlink "${dot}/.config/yazi/flavors/ashen.yazi";
  xdg.configFile."yazi/flavors/kanagawa-dragon.yazi".source = config.lib.file.mkOutOfStoreSymlink "${dot}/.config/yazi/flavors/kanagawa-dragon.yazi";
  xdg.configFile."yazi/plugins/sudo.yazi".source = config.lib.file.mkOutOfStoreSymlink "${dot}/.config/yazi/plugins/sudo.yazi";
}
