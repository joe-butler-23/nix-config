{user, ...}: {
  # Syncthing file synchronization service
  services.syncthing = {
    enable = true;
    inherit user;
    dataDir = "/home/${user}/.local/share/syncthing";
    configDir = "/home/${user}/.config/syncthing";
  };
}
