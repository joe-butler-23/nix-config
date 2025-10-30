# modules/packages/core.nix
{
  ...
}: {
  environment.systemPackages = with pkgs; [
    # Core desktop utils
    foot
    capitaine-cursors
    bluetui
    localsend
    viewnior
    xfce.mousepad
    xfce.thunar
    gvfs
    papirus-icon-theme
    hicolor-icon-theme
    desktop-file-utils

    # Shell / CLI tools
    bat
    ripgrep
    fd
    fzf
    zoxide
    starship
  ];
}
