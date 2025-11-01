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
  ];

  #### User identity
  home.username = "joebutler";
  home.homeDirectory = "/home/joebutler";
  home.stateVersion = "25.05";

  #### Fonts and theming
  fonts = {
    fontconfig.enable = true;
    packages = with pkgs; [
      jetbrains-mono
      nerd-fonts.jetbrains-mono
      font-awesome
      noto-fonts-emoji
    ];
  };

  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
    };
    font.name = "Noto Sans 10";
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

  #### Shell configuration
  home.file.".zshenv".source = OOS "${dot}/.zshenv";
  home.file.".zprofile".source = OOS "${dot}/.zprofile";
  home.file.".zshrc".source = OOS "${dot}/.zshrc";
  programs.zsh.enable = true;

  #### Application configuration links
  xdg.configFile."hypr".source = OOS "${dot}/.config/hypr";
  xdg.configFile."waybar".source = OOS "${dot}/.config/waybar";
  xdg.configFile."foot".source = OOS "${dot}/.config/foot";
  xdg.configFile."rofi".source = OOS "${dot}/.config/rofi";
  xdg.configFile."yazi".source = OOS "${dot}/.config/yazi";
  xdg.configFile."zathura".source = OOS "${dot}/.config/zathura";
  xdg.configFile."lazygit".source = OOS "${dot}/.config/lazygit";
  xdg.configFile."wlogout".source = OOS "${dot}/.config/wlogout";
  xdg.configFile."mako".source = OOS "${dot}/.config/mako";

  #### Wayland compositor
  wayland.windowManager.hyprland.enable = true;
}
