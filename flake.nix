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
    nixosConfigurations = {
      # Laptop (auto-selected when hostname == "laptop-nix")
      laptop-nix = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit pkgsUnstable;};
        modules = [
          ./configuration.nix
          stylix.nixosModules.stylix

          # Home Manager integrated as before
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-bak";
            home-manager.extraSpecialArgs = {inherit pkgsUnstable;};
            home-manager.users.joebutler = import ./home.nix;
          }

          # Laptop-only deltas (kanshi + hostname)
          ./modules/hosts/laptop-nix.nix
        ];
      };

      # Desktop (auto-selected when hostname == "desktop-nix")
      desktop-nix = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit pkgsUnstable;};
        modules = [
          ./configuration.nix
          stylix.nixosModules.stylix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-bak";
            home-manager.extraSpecialArgs = {inherit pkgsUnstable;};
            home-manager.users.joebutler = import ./home.nix;
          }

          # Desktop-only deltas (hostname for now)
          ./modules/hosts/desktop-nix.nix
        ];
      };
    };

    # formatter unchanged
    formatter.${system} =
      treefmt-nix.lib.mkWrapper
      nixpkgs.legacyPackages.${system}
      (import ./treefmt.nix);
  };
}
