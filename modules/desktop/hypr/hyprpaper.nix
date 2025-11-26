{config, ...}: let
  wallpaper = "${config.home.homeDirectory}/nix-config/assets/wallpaper.jpeg";
in {
  # Hyprpaper - wallpaper management
  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = {on      };
      preload = [wallpaper];
      wallpaper = [",${wallpaper}"];
    };
  };
}
