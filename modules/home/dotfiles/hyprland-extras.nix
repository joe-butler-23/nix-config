{config, ...}: let
  dot = "${config.home.homeDirectory}/.dotfiles";
in {
  # Hyprland additional configuration files
  xdg.configFile."hypr/hypridle.conf".source = config.lib.file.mkOutOfStoreSymlink "${dot}/.config/hypr/hypridle.conf";
  xdg.configFile."hypr/hyprlock.conf".source = config.lib.file.mkOutOfStoreSymlink "${dot}/.config/hypr/hyprlock.conf";
  xdg.configFile."hypr/hyprpaper.conf".source = config.lib.file.mkOutOfStoreSymlink "${dot}/.config/hypr/hyprpaper.conf";
  xdg.configFile."hypr/toggle_eDP1.sh".source = config.lib.file.mkOutOfStoreSymlink "${dot}/.config/hypr/toggle_eDP1.sh";
  xdg.configFile."hypr/auto_toggle_eDP1.sh".source = config.lib.file.mkOutOfStoreSymlink "${dot}/.config/hypr/auto_toggle_eDP1.sh";
}
