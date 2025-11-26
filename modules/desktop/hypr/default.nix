{pkgs, ...}: {
  imports = [
    ./hyprland
    ./hypridle.nix
    ./hyprlock.nix
  ];

  home.packages = [pkgs.swaybg];
}
