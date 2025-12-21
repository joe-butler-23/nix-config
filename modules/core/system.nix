{pkgs, ...}: {
  imports = [
  ];

  #### Hyprland configuration (System)
  programs.hyprland.enable = true;
  hardware.graphics.enable = true;
  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
    EDITOR = "emacsclient -c -n";
    VISUAL = "emacsclient -c -n";
    SOPS_EDITOR = "emacsclient -t";
  };

  #### Display manager
  services.displayManager.sddm.enable = false;
  services.displayManager.ly = {
    enable = true;
  };

  #### XDG Portals for Wayland (Hyprland + GTK)
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
    ];
  };
}
