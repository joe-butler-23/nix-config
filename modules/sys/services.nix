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
    extraGroups = ["networkmanager" "wheel"];
    shell = pkgs.zsh;
  };

  #### Shell configuration
  programs.zsh.enable = true;

  #### Power management
  powerManagement = {
    enable = true;
  };

  #### Networking
  networking.hostName = "nixos";
  networking.networkmanager.enable = true;
  networking.firewall = {
    enable = true;
  };

  # Add custom Plymouth theme
  environment.systemPackages = [
    pkgs.plymouth
  ];

  # Create symlink for custom Plymouth theme
  systemd.services."plymouth-hold".wantedBy = ["multi-user.target"];

  # Copy Plymouth theme to system location
  system.activationScripts.setupPlymouthTheme.text = ''
    #!/bin/sh
    mkdir -p /usr/share/plymouth/themes/ifruit-custom
    cp -r ${../plymouth/ifruit-custom}/* /usr/share/plymouth/themes/ifruit-custom/
    chmod 644 /usr/share/plymouth/themes/ifruit-custom/*
    plymouth-set-default-theme ifruit-custom
  '';
}
