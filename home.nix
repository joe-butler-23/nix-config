{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (lib) flatten flip map mergeAttrsList;

  # Your dotfiles path
  symlinkRoot = "${config.home.homeDirectory}/.dotfiles";

  pipe = flip lib.pipe;
  flatMerge = pipe [flatten mergeAttrsList];

  toSrcFile = name: "${symlinkRoot}/${name}";
  link = pipe [toSrcFile mkOutOfStoreSymlink];

  linkDir = name: {
    ${name} = {
      source = link name;
      recursive = true;
    };
  };

  linkConfDirs = map linkDir;

  # Config directories
  confDirs = linkConfDirs [
    "hypr"
    "waybar"
    "foot"
    "rofi"
    "yazi"
    "zathura"
    "lazygit"
    "wlogout"
  ];

  links = flatMerge confDirs;
in {
  home.username = "joebutler";
  home.homeDirectory = "/home/joebutler";
  home.stateVersion = "25.05";

  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
    };
    font.name = "Noto Sans 10";
  };

  # Top-level files
  home.file.".zshenv".source = mkOutOfStoreSymlink "${symlinkRoot}/.zshenv";
  home.file.".zprofile".source = mkOutOfStoreSymlink "${symlinkRoot}/.zprofile";
  home.file.".zshrc".source = mkOutOfStoreSymlink "${symlinkRoot}/.zshrc";

  # Config directories use new structure
  xdg.configFile = links;

  programs.zsh.enable = true;
}
