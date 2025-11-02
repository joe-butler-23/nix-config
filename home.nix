{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./modules/home/packages.nix
    ./modules/home/services.nix
    ./modules/home/dotfiles
  ];

  #### User identity
  home.username = "joebutler";
  home.homeDirectory = "/home/joebutler";
  home.stateVersion = "25.05";

  #### Fonts and theming
  fonts = {
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = ["JetBrains Mono"];
        sansSerif = ["Noto Sans"];
        serif = ["Noto Serif"];
        emoji = ["Noto Color Emoji"];
      };
    };
  };

  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
    };
    font = {
      name = "Noto Sans";
      size = 10;
    };
  };

  home.pointerCursor = {
    name = "capitaine-cursors";
    size = 24;
    package = pkgs.capitaine-cursors;
    gtk.enable = true;
  };

  #### Session variables
  home.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
  };
}
