{
  config,
  pkgs,
  ...
}: let
  OOS = config.lib.file.mkOutOfStoreSymlink;
  dot = "${config.home.homeDirectory}/.dotfiles";
in {
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

  #### Application configuration links
  xdg.configFile."waybar".source = OOS "${dot}/.config/waybar";
  xdg.configFile."foot".source = OOS "${dot}/.config/foot";
  xdg.configFile."rofi".source = OOS "${dot}/.config/rofi";
  xdg.configFile."yazi".source = OOS "${dot}/.config/yazi";
  xdg.configFile."zathura".source = OOS "${dot}/.config/zathura";
  xdg.configFile."lazygit".source = OOS "${dot}/.config/lazygit";
  xdg.configFile."wlogout".source = OOS "${dot}/.config/wlogout";
  xdg.configFile."mako".source = OOS "${dot}/.config/mako";

  #### Hypr additional config files (hyprland.conf is managed by the hyprland module)
  xdg.configFile."hypr/hypridle.conf".source = OOS "${dot}/.config/hypr/hypridle.conf";
  xdg.configFile."hypr/hyprlock.conf".source = OOS "${dot}/.config/hypr/hyprlock.conf";
  xdg.configFile."hypr/hyprpaper.conf".source = OOS "${dot}/.config/hypr/hyprpaper.conf";
  xdg.configFile."hypr/toggle_eDP1.sh".source = OOS "${dot}/.config/hypr/toggle_eDP1.sh";
  xdg.configFile."hypr/auto_toggle_eDP1.sh".source = OOS "${dot}/.config/hypr/auto_toggle_eDP1.sh";
}
