# modules/services.nix
{ config, lib, pkgs, ... }:
{
  #### Display manager and session
  services.displayManager.sddm.enable = false;

  services.displayManager.emptty = {
    enable = true;
    defaultUser = "me";
    defaultSession = "hyprland";
  };

  programs.hyprland.enable = true;

  #### PolicyKit
  security.polkit.enable = true;

  #### Networking (NetworkManager with iwd backend)
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  #### Audio (PipeWire + WirePlumber)
  hardware.pulseaudio.enable = false;
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
