# modules/home/services.nix
_: {
  #### Syncthing service - temporarily disabled to fix rebuild hanging
  services.syncthing = {
    enable = false;
    tray.enable = false;
  };
}
