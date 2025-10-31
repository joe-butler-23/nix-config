{
  config,
  pkgs,
  lib,
  ...
}: let
  OOS = config.lib.file.mkOutOfStoreSymlink;
  dot = "${config.home.homeDirectory}/.dotfiles";
in {
  home.username = "joebutler";
  home.homeDirectory = "/home/joebutler";
  home.stateVersion = "25.05";

  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
    };
    font.name = "Noto Sans 10";
  };

  # Link top-level Zsh files from ~/.dotfiles
  home.file.".zshenv".source = OOS "${dot}/.zshenv";
  home.file.".zprofile".source = OOS "${dot}/.zprofile";
  home.file.".zshrc".source = OOS "${dot}/.zshrc";

  # Link configs
  xdg.configFile."hypr".source = OOS "${dot}/.config/hypr";
  xdg.configFile."waybar".source = OOS "${dot}/.config/waybar";
  xdg.configFile."foot".source = OOS "${dot}/.config/foot";
  xdg.configFile."rofi".source = OOS "${dot}/.config/rofi";
  xdg.configFile."yazi".source = OOS "${dot}/.config/yazi";
  xdg.configFile."zathura".source = OOS "${dot}/.config/zathura";
  xdg.configFile."lazygit".source = OOS "${dot}/.config/lazygit";
  xdg.configFile."wlogout".source = OOS "${dot}/.config/wlogout";

  # Enable Zsh (uses your linked files above)
  programs.zsh.enable = true;
}
