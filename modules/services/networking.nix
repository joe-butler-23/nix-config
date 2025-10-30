# modules/services/networking.nix
{
  ...
}: {
  #### Syncthing
  services.syncthing = {
    enable = true;
    user = "me";
    dataDir = "/home/me"; # Default folder base
    configDir = "/home/me/.config/syncthing";
  };

  #### SSH server
  services.openssh.enable = true;
}
