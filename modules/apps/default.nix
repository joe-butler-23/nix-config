{...}: {
  imports = [
    ./apps.nix
    ./dev.nix
    ./anki.nix
    ./anki-forge.nix
    ./vscodium.nix
    ./thunar.nix
    ./utils.nix
    ./web-apps
  ];

  systemd.user.startServices = "sd-switch";
}
