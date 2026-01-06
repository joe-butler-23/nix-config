{...}: {
  imports = [
    ./espanso.nix
    ./hypridle.nix
    ./syncthing.nix
    ./overlay-updates.nix
    # kanshi: laptop-specific (modules/hosts/laptop-nix.nix)
  ];
}
