{pkgsUnstable, ...}: {
  imports = [
    ./binds.nix
    ./rules.nix
    ./settings.nix
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    package = pkgsUnstable.hyprland;
    xwayland.enable = true;
  };
}
