{
  pkgs,
  pkgsUnstable,
  ...
}: {
  imports = [
    ./binds.nix
    ./rules.nix
    ./settings.nix
  ];

  home.packages = [pkgs.swaybg];

  wayland.windowManager.hyprland = {
    enable = true;
    package = pkgsUnstable.hyprland;
    xwayland.enable = true;
  };
}
