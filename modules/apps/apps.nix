{pkgs, ...}: {
  home.packages = with pkgs; [
    gnumeric
    obsidian
    zotero
    vivaldi
  ];
}
