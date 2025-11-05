# modules/home/whichkey.nix
{ pkgs, whichkey, ... }: {
  home.packages = [
    whichkey.packages.x86_64-linux.wlr-which-key
  ];

  home.configFile."wlr-which-key/config.yaml".source = 
    /home/joebutler/development/whichkey/config/default.yaml;
}
