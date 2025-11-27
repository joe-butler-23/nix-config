{...}: {
  imports = [
    ./apps.nix
    ./dev.nix
    ./anki.nix
    ./anki-forge.nix
    ./vscodium.nix
    ./thunar.nix
    ./utils.nix
    ./espanso.nix
    ./zathura.nix
    ./syncthing.nix
    ./web-apps
  ];

  systemd.user.startServices = "sd-switch";
}
