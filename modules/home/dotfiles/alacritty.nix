{ ... }:

{
  programs.alacritty = {
    enable = true;

    settings = {
      # Font: JetBrainsMono Nerd Font:size=10
      font = {
        normal = {
          family = "JetBrainsMono Nerd Font";
        };
        size = 10.0;
      };

      # Scrollback: lines = 10000
      scrolling = {
        history = 10000;
      };

      # Cursor: style = "beam"
      cursor = {
        style = "Beam";
      };

      # Opacity: alpha = "1.0"
      window = {
        opacity = 1.0;
      };

      # Nord dark theme
      colors = {
        primary = {
          background = "0x2e3440";
          foreground = "0xd8dee9";
        };

        normal = {
          black   = "0x3b4252"; # regular0
          red     = "0xbf616a"; # regular1
          green   = "0xa3be8c"; # regular2
          yellow  = "0xebcb8b"; # regular3
          blue    = "0x81a1c1"; # regular4
          magenta = "0xb48ead"; # regular5
          cyan    = "0x88c0d0"; # regular6
          white   = "0xe5e9f0"; # regular7
        };

        bright = {
          black   = "0x4c566a"; # bright0
          red     = "0xbf616a"; # bright1
          green   = "0xa3be8c"; # bright2
          yellow  = "0xebcb8b"; # bright3
          blue    = "0x81a1c1"; # bright4
          magenta = "0xb48ead"; # bright5
          cyan    = "0x8fbcbb"; # bright6
          white   = "0xeceff4"; # bright7
        };

        dim = {
          black   = "0x373e4d"; # dim0
          red     = "0x94545d"; # dim1
          green   = "0x809575"; # dim2
          yellow  = "0xb29e75"; # dim3
          blue    = "0x68809a"; # dim4
          magenta = "0x8c738c"; # dim5
          cyan    = "0x6d96a5"; # dim6
          white   = "0xaeb3bb"; # dim7
        };
      };
    };
  };
}
