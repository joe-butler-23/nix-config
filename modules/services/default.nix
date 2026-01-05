{...}: {
  imports = [
    ./espanso.nix
    ./hypridle.nix
    ./syncthing.nix
    ./rclone-mount.nix
    ./overlay-updates.nix
    # kanshi: laptop-specific (modules/hosts/laptop-nix.nix)
  ];
}
