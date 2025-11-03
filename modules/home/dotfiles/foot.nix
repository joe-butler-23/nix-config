_: {
  programs.foot = {
    enable = true;

    settings = {
      main = {
        font = "JetBrainsMono Nerd Font:size=10";
      };

      scrollback = {
        lines = 10000;
      };

      cursor = {
        style = "beam";
      };

      # Nord Light theme - overrides Stylix theming
      colors = {
        foreground = "2E3440";
        background = "D8DEE9";

        regular0 = "3B4252"; # black
        regular1 = "6b363b"; # red
        regular2 = "4f673a"; # green
        regular3 = "968259"; # yellow
        regular4 = "334d67"; # blue
        regular5 = "5c3d57"; # magenta
        regular6 = "3d6665"; # cyan
        regular7 = "e5e9f0"; # white

        bright0 = "4c566a"; # bright black
        bright1 = "bf616a"; # bright red
        bright2 = "a3be8c"; # bright green
        bright3 = "ebcb8b"; # bright yellow
        bright4 = "81a1c1"; # bright blue
        bright5 = "b48ead"; # bright magenta
        bright6 = "8fbcbb"; # bright cyan
        bright7 = "eceff4"; # bright white
      };
    };
  };
}
