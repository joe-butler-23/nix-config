# modules/packages/desktop.nix
{
  ...
}: {
  environment.systemPackages = with pkgs; [
    # Wayland / Hyprland
    hypridle
    hyprlock
    hyprshot
    hyprland-protocols
    rofi-wayland
    swaynotificationcenter
    waybar
    wl-clipboard
    xdg-desktop-portal-hyprland
    uwsm
    wlogout
  ];
}
