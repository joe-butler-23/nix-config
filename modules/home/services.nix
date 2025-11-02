# modules/home/services.nix
_: {
  #### Syncthing - file synchronization (private config via env vars)
  services.syncthing = {
    enable = true;
    tray.enable = true;

    # Use environment variables for private configuration
    # Set these in ~/.config/syncthing-env before running home-manager switch
    # SYNCTHING_DEVICE_ID=your-device-id
    # SYNCTHING_DEVICES={"device1":{"id":"ID1"},"device2":{"id":"ID2"}}
    # SYNCTHING_FOLDERS={"folder1":{"path":"~/folder1","devices":["device1"]}}

    # Parse JSON from environment variables if set, otherwise empty
    devices =
      if builtins.getEnv "SYNCTHING_DEVICES" != ""
      then builtins.fromJSON (builtins.getEnv "SYNCTHING_DEVICES")
      else {};

    folders =
      if builtins.getEnv "SYNCTHING_FOLDERS" != ""
      then builtins.fromJSON (builtins.getEnv "SYNCTHING_FOLDERS")
      else {};

    # Override device ID if provided
    overrideDevices = builtins.getEnv "SYNCTHING_DEVICE_ID" != "";
    overrideFolders = builtins.getEnv "SYNCTHING_FOLDERS" != "";

    # Basic settings
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
