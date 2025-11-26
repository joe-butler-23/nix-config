{config, ...}: let
  wallpaper = "${config.home.homeDirectory}/nix-config/assets/wallpaper.jpeg";
in {
  # Hyprpaper - wallpaper management
  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = {
        on_shutdown = ["hyprctl dispatch dpms on"];
      };
      preload = [wallpaper];
      wallpaper = [",${wallpaper}"];
    };
  };
}
