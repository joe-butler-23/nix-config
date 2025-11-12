{ pkgs, ... }: {
  home.packages = [
    (import ./file-launcher.nix { inherit pkgs; })
    (import ./recent-files-launcher.nix { inherit pkgs; })
  ];
}
