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

      # Nord dark theme - overrides Stylix theming
      colors = {
        foreground = "d8dee9";
        background = "2e3440";

        regular0 = "3b4252"; # black
        regular1 = "bf616a"; # red
        regular2 = "a3be8c"; # green
        regular3 = "ebcb8b"; # yellow
        regular4 = "81a1c1"; # blue
        regular5 = "b48ead"; # magenta
        regular6 = "88c0d0"; # cyan
        regular7 = "e5e9f0"; # white

        bright0 = "4c566a"; # bright black
        bright1 = "bf616a"; # bright red
        bright2 = "a3be8c"; # bright green
        bright3 = "ebcb8b"; # bright yellow
        bright4 = "81a1c1"; # bright blue
        bright5 = "b48ead"; # bright magenta
        bright6 = "8fbcbb"; # bright cyan
        bright7 = "eceff4"; # bright white

        dim0 = "373e4d"; # dim black
        dim1 = "94545d"; # dim red
        dim2 = "809575"; # dim green
        dim3 = "b29e75"; # dim yellow
        dim4 = "68809a"; # dim blue
        dim5 = "8c738c"; # dim magenta
        dim6 = "6d96a5"; # dim cyan
        dim7 = "aeb3bb"; # dim white
      };

      "key-bindings" = {
        # Copy last command output to clipboard with Ctrl+Space
        "pipe-command-output" = "[wl-copy] Control+Shift+b";
      };
    };
  };
}
