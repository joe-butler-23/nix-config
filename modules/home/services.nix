# modules/home/services.nix
_: {
  #### Syncthing - file synchronization
  services.syncthing = {
    enable = true;
    tray.enable = true;
  };
}
