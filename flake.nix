{
  description = "My NixOS Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager";
    # Make HM use the same nixpkgs as your system by default
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }:
  let
    system = "x86_64-linux";
    pkgs         = import nixpkgs         { inherit system; };
    pkgsUnstable = import nixpkgs-unstable { inherit system; };
  in
  {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      inherit system;

      # Make pkgsUnstable available to modules (you already use this)
      specialArgs = { inherit pkgsUnstable; };

      modules = [
        ./configuration.nix

        # Enable Home Manager as a NixOS module
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;   # use the pkgs from this system
          home-manager.useUserPackages = true; # install user pkgs via HM

          # Define your user’s HM config (create ./home.nix next)
          home-manager.users.me = import ./home.nix;
        }
      ];
    };
  };
}
