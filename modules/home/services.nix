# modules/home/services.nix
{config, ...}: {
  #### User services
  services.syncthing = {
    enable = true;
    dataDir = config.home.homeDirectory;
    configDir = "${config.home.homeDirectory}/.config/syncthing";
  };
}
