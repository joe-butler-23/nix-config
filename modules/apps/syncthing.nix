# modules/home/services.nix
_: {
  #### Syncthing - file synchronization
  services.syncthing = {
    enable = true;
    tray.enable = true;
  };

  # Fix Qt platform plugin for syncthingtray
  home.sessionVariables = {
    QT_QPA_PLATFORM = "wayland";
  };
}
