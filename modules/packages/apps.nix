# modules/packages/apps.nix
{
  ...
}: {
  environment.systemPackages = with pkgs; [
    # Apps
    anki-bin
    espanso-wayland
    obsidian
    vscodium
    zotero
    zathura
  ];
}
