# modules/home/services.nix
{pkgs, ...}: {
  #### Syncthing - file synchronization
  services.syncthing = {
    enable = true;
    tray.enable = true;
  };
}
