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
  hardware.bluetooth.settings = {
    General = {
      Enable = "Source,Sink,Media,Socket,HID";
    };
  };
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

  #### SSH server
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
    '';
  };

  #### Tailscale VPN
  services.tailscale = {
    enable = true;
    openFirewall = true;
  };

  #### Performance services
  services.irqbalance.enable = true;
  services.fstrim.enable = true;

  #### Hyprland configuration
  programs.hyprland.enable = true;
  hardware.graphics.enable = true;
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  #### User account
  users.users.joebutler = {
    isNormalUser = true;
    createHome = true;
    extraGroups = ["networkmanager" "wheel" "input" "docker"];
    shell = pkgs.zsh;
  };

  #### Docker
  virtualisation.docker.enable = true;

  #### Uinput for Espanso
  services.udev.extraRules = ''
    KERNEL=="uinput", MODE="0660", GROUP="input", OPTIONS+="static_node=uinput"
  '';

  #### Shell configuration
  programs.zsh.enable = true;

  #### Power management
  powerManagement = {
    enable = true;
  };

  #### Networking
  networking.networkmanager.enable = true;
  networking.firewall = {
    enable = true;
  };
}
