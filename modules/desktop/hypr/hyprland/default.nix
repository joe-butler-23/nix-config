{
  pkgs,
  pkgsUnstable,
  ...
}: {
  # Config now managed by chezmoi at ~/.config/hypr/hyprland.conf
  home.packages = [pkgs.swaybg pkgs.rofi pkgs.mako];

  wayland.windowManager.hyprland = {
    enable = true;
    package = pkgsUnstable.hyprland;
    xwayland.enable = true;
  };
}
