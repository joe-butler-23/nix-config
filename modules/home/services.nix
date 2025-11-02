# modules/home/services.nix
_: {
  #### Syncthing service
  services.syncthing = {
    enable = true;
    tray.enable = true;
    guiAddress = "127.0.0.1:8384";
    settings = {
      options = {
        localAnnounceEnabled = true;
        globalAnnounceEnabled = true;
        relaysEnabled = true;
        urAccepted = -1; # Disable usage reporting
      };
      devices = {
        # Add your devices here
        # Example:
        # "phone" = {
        #   id = "DEVICE-ID-HERE";
        #   name = "Phone";
        #   introducer = false;
        # };
      };
      folders = {
        # Add your folders here
        # Example:
        # "Documents" = {
        #   enable = true;
        #   path = "~/Documents";
        #   devices = [ "phone" ];
        #   versioning = {
        #     type = "trashcan";
        #     params.cleanoutDays = 30;
        #   };
        # };
      };
    };
  };
}
