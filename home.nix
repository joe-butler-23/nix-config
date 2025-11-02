{pkgs, ...}: {
  imports = [
    ./modules/home/packages.nix
    ./modules/home/services.nix
    ./modules/home/dotfiles
  ];

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
