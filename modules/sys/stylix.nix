{
  pkgs,
  ...
}: {
  stylix = {
    enable = true;

    # Wallpaper - generates color scheme automatically
    image = /home/joebutler/Pictures/gradient.jpeg;

    # Image polarity - affects color generation
    polarity = "dark";

    # Font configuration - matching your existing setup
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
        desktop = 10;
        popups = 10;
        terminal = 11;
      };
    };

    # Cursor configuration - matching your existing setup
    cursor = {
      name = "capitaine-cursors";
      package = pkgs.capitaine-cursors;
      size = 24;
    };

    # Optional: Terminal opacity
    opacity = {
      terminal = 0.95;
    };
  };
}
