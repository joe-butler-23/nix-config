{pkgs, ...}: {
  imports = [
    ./stylix.nix
  ];

  #### Hyprland sys setup
  programs.hyprland.enable = true;
  hardware.graphics.enable = true;
  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
    GDK_BACKEND = "wayland";
  };

  #### Display manager
  services.displayManager.sddm.enable = false;
  services.displayManager.ly = {
    enable = true;
  };

  #### Desktop plumbing
  services.gvfs.enable = true;
  services.tumbler.enable = true;

  #### XDG Portals for Wayland
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
    ];
  };
}
