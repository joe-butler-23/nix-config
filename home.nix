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

  # Stylix theming
  stylix.targets = {
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

  #### Session variables
  home.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
  };
}
