# modules/home/services.nix
_: {
  #### Syncthing - file synchronization
  services.syncthing = {
    enable = true;
    tray.enable = true;

    # Basic settings - configure devices/folders via web UI at http://localhost:8384
    extraOptions = {
      gui = {
        theme = "dark";
        insecureAdminAccess = false;
      };
      options = {
        globalAnnounceEnabled = false;
        localAnnounceEnabled = true;
        relaysEnabled = false;
        natEnabled = true;
      };
    };
  };
}
