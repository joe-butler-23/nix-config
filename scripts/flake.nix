{
  description = "Nix-based file launcher";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default = pkgs.callPackage ./file-launcher.nix { };
      packages.${system}.recent-files-launcher = pkgs.callPackage ./recent-files-launcher.nix { };
      
      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/fzf-file-launcher";
      };
      
      apps.${system}.recent-files = {
        type = "app";
        program = "${self.packages.${system}.recent-files-launcher}/bin/recent-files-launcher";
      };
    };
}
