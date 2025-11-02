{ config, ... }:

{
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
        style = "block";
      };
    };
  };
}
