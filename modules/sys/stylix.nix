{pkgs, ...}: {
  stylix = {
    enable = true;

    # Custom Nord color palette
    base16Scheme = {
      base00 = "1d2129"; # background
      base01 = "292e39"; # regular0 (black)
      base02 = "4c566a"; # bright0 (bright black)
      base03 = "434c5e";
      base04 = "d8dee9"; # foreground
      base05 = "e5e9f0"; # regular7 (white)
      base06 = "eceff4"; # bright7 (bright white)
      base07 = "8fbcbb"; # bright6 (bright cyan)
      base08 = "bf616a"; # regular1 (red)
      base09 = "d08770";
      base0A = "ebcb8b"; # regular3 (yellow)
      base0B = "a3be8c"; # regular2 (green)
      base0C = "88c0d0"; # regular6 (cyan)
      base0D = "81a1c1"; # regular4 (blue)
      base0E = "b48ead"; # regular5 (magenta)
      base0F = "5e81ac";
    };

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
        desktop = 12;
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
