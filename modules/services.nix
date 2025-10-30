# modules/services.nix
{
  pkgs,
  ...
}: {
  #### Display manager and session
  services.displayManager.sddm.enable = false;

  services.displayManager.ly = {
    enable = true;
  };

  #### PolicyKit
  security.polkit.enable = true;

  #### Syncthing
  services.syncthing = {
    enable = true;
    user = "me";
    dataDir = "/home/me"; # Default folder base
    configDir = "/home/me/.config/syncthing";
  };

  #### Audio
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  #### Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  #### XDG Portals for Wayland (Hyprland + GTK)
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
    ];
  };

  #### SSH server
  services.openssh.enable = true;

  #### Power management
  services.tlp.enable = true;
}
