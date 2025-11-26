{...}: {
  imports = [
    ./gtk.nix
    ./hypr
    ./mako.nix
    ./rofi.nix
    ./waybar.nix
    ./whichkey.nix
    ./wlogout/wlogout.nix
  ];

  services.blueman-applet.enable = true;
  services.clipse.enable = true;
}
