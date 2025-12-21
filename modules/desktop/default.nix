{...}: {
  imports = [
    ./mime-types.nix
    ./system.nix
    ./brave-wrapper.nix
    ../apps/web-apps/default.nix
  ];
}
