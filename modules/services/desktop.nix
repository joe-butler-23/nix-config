# modules/services/desktop.nix
{
  pkgs,
  ...
}: {
  #### Display manager and session
  services.displayManager.sddm.enable = false;

  services.displayManager.ly = {
    enable = true;
  };
}
