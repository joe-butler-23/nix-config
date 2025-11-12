# modules/home/anki.nix
{
  pkgs,
  pkgsUnstable,
  config,
  lib,
  ...
}: {
  programs.anki = {
    enable = true;
    package = pkgsUnstable.anki;
    
    # Use anki-connect from unstable
    addons = [
      pkgsUnstable.ankiAddons.anki-connect
    ];
  };
}
