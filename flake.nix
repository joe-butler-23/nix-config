{
  description = "My NixOS Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      # Track 25.05 HM release; follows your nixpkgs
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    stylix,
    treefmt-nix,
    ...
  }: let
    system = "x86_64-linux";
    pkgsUnstable = import nixpkgs-unstable {inherit system;};
  in {
    # Standalone home-manager configuration removed to avoid conflicts
    # Use the integrated configuration in nixosConfigurations instead

    # Main NixOS system (used by `sudo nixos-rebuild switch --flake ...`)
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      inherit system;

      # Make pkgsUnstable available to your modules
      specialArgs = {inherit pkgsUnstable;};

      modules = [
        ./configuration.nix
        stylix.nixosModules.stylix

        # Integrate Home Manager into the system build
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true; # share pkgs with system
          home-manager.useUserPackages = true; # install user pkgs via HM
          home-manager.backupFileExtension = "hm-bak";
          home-manager.extraSpecialArgs = {inherit pkgsUnstable;};

          # Activate Home Manager for user "joebutler"
          home-manager.users.joebutler = import ./home.nix;
        }
      ];
    };

    # Format and lint Nix files (treefmt)
    formatter.${system} =
      treefmt-nix.lib.mkWrapper
      nixpkgs.legacyPackages.${system}
      (import ./treefmt.nix);
  };
}
