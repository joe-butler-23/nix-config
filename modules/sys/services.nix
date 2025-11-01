# modules/sys/services.nix
{pkgs, ...}: {
  #### Display manager and session
  services.displayManager.sddm.enable = false;

  services.displayManager.ly = {
    enable = true;
  };

  #### PolicyKit
  security.polkit.enable = true;

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

  #### Desktop plumbing
  services.gvfs.enable = true;
  services.tumbler.enable = true;

  #### XDG Portals for Wayland (Hyprland + GTK)
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
    ];
  };

  #### SSH server (hardened for Tailscale compatibility)
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
      # Listening on all interfaces, Tailscale manages network access via firewall
    '';
  };

  #### Tailscale VPN
  services.tailscale = {
    enable = true;
    openFirewall = true; # Let tailscaled manage firewall rules
  };

  #### Performance services
  services.irqbalance.enable = true;
  services.fstrim.enable = true;
}
