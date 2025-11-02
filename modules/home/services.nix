# modules/home/services.nix
_: {
  #### Syncthing - file synchronization
  services.syncthing = {
    enable = true;
    tray.enable = true;

    # Configure via web UI at http://localhost:8384
    # - Add devices and folders through the web interface
    # - Settings are persisted in ~/.config/syncthing/
  };
}
