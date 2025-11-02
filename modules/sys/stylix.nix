{
  pkgs,
  ...
}: {
  stylix = {
    enable = true;

    # Nord Light color scheme
    base16Scheme = "${pkgs.base16-schemes}/share/themes/nord-light.yaml";

    # Wallpaper (relative path for flake purity)
    image = ./../../wallpaper.jpeg;

    # Font configuration
    fonts = {
      monospace = {
        name = "JetBrainsMono Nerd Font";
        package = pkgs.nerd-fonts.jetbrains-mono;
      };
      sansSerif = {
        name = "Noto Sans";
        package = pkgs.noto-fonts;
      };
      serif = {
        name = "Noto Serif";
        package = pkgs.noto-fonts;
      };
      emoji = {
        name = "Noto Color Emoji";
        package = pkgs.noto-fonts-emoji;
      };

      sizes = {
        applications = 10;
        desktop = 12; # Waybar uses desktop size
        popups = 10;
        terminal = 11;
      };
    };

    # Cursor configuration
    cursor = {
      name = "capitaine-cursors";
      package = pkgs.capitaine-cursors;
      size = 24;
    };

    # Terminal opacity
    opacity = {
      terminal = 0.95;
    };
  };
}
