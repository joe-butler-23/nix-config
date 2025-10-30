# modules/services.nix
{pkgs, ...}: {
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
    user = "joebutler";
    dataDir = "/home/joebutler"; # Default folder base
    configDir = "/home/joebutler/.config/syncthing";
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

  #### SSH server (hardened for Tailscale)
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      AllowTcpForwarding = false;
      X11Forwarding = false;
      LogLevel = "VERBOSE";
      MaxAuthTries = 3;
      LoginGraceTime = 20;
    };
    extraConfig = ''
      AllowUsers joebutler
      ListenAddress 127.0.0.1
      ListenAddress 100.64.0.3
    '';
  };

  #### Tailscale VPN
  services.tailscale.enable = true;

  #### Performance services
  services = {
    irqbalance.enable = true;
    fstrim.enable = true;
  };
}
