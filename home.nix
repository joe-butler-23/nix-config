{pkgs, ...}: {
  imports = [
    ./modules/home/packages.nix
    ./modules/home/services.nix
    ./modules/home/dotfiles
  ];

  # Import Stylix Home Manager module (required due to breaking change)
  stylix = {
    enable = true;

    # Nord Light color scheme
    base16Scheme = "${pkgs.base16-schemes}/share/themes/nord-light.yaml";

    # Wallpaper (relative path for flake purity)
    image = ./wallpaper.jpeg;

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

  #### User identity
  home.username = "joebutler";
  home.homeDirectory = "/home/joebutler";
  home.stateVersion = "25.05";

  # Icon theme (Stylix doesn't handle icons, fonts managed by Stylix)
  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
  };

  # Disable Stylix theming for apps with manual themes
  stylix.targets = {
    foot.enable = false;    # Manual Nord theme in foot.nix
    mako.enable = false;    # Manual theme in mako.nix
    hyprlock.enable = false; # Manual theme in hyprland-extras.nix
    yazi.enable = false;    # Manual Kanagawa theme in yazi.nix
  };

  #### Session variables
  home.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
  };
}
