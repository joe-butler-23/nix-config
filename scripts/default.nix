{ pkgs, ... }: {
  home.packages = [
    (import ./file-launcher.nix { inherit pkgs; })
    (import ./recent-files-launcher.nix { inherit pkgs; })
    (import ./copy-prompt.nix { inherit pkgs; })
    (import ./directory-finder.nix { inherit pkgs; })
  ];
}
