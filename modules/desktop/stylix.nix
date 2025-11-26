{
  pkgs,
  user,
  ...
}: {
  stylix = {
    enable = true;
    autoEnable = false;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/nord-light.yaml";
    targets.plymouth.enable = false;
    image = ./../../assets/wallpaper.jpeg;

    cursor = {
      name = "capitaine-cursors";
      package = pkgs.capitaine-cursors;
      size = 24;
    };

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

    opacity = {
      terminal = 0.95;
    };
  };

  # Home Manager Targets
  home-manager.users.${user}.stylix.targets = {
    # False
    foot.enable = false;
    mako.enable = false;
    yazi.enable = false;
    waybar.enable = false;

    # True
    rofi.enable = true;
    gtk.enable = true;
    hyprlock.enable = true;
  };
}
