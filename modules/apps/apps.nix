{pkgs, ...}: {
  home.packages = with pkgs; [
    brave
    gnumeric
    obsidian
    zotero
  ];
}
