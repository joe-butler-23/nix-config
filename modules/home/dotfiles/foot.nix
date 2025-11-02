_: {
  programs.foot = {
    enable = true;

    settings = {
      main = {
        # Font - commented out to allow Stylix font management
        # font = "JetBrainsMono Nerd Font:size=10";
      };

      scrollback = {
        lines = 10000;
      };

      cursor = {
        style = "block";
      };
    };
  };
}
