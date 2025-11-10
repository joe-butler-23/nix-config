# flake.nix
{
  description = "My NixOS Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
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

    # NEW: generated VS Code/VSCodium extensions (Open VSX + Marketplace)
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    nix-vscode-extensions.inputs.nixpkgs.follows = "nixpkgs";

    # Local whichkey development flake
    whichkey.url = "git+file:///home/joebutler/development/whichkey";
  };

  outputs = {
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    stylix,
    treefmt-nix,
    nix-vscode-extensions,
    whichkey,
    ...
  }: let
    system = "x86_64-linux";
    pkgsUnstable = import nixpkgs-unstable {inherit system;};
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    # Generated extensions set (kept up-to-date by upstream CI)
    vsx = nix-vscode-extensions.extensions.${system}.vscode-marketplace;
  in {
    nixosConfigurations = {
      # Laptop (auto-selected when hostname == "laptop-nix")
      laptop-nix = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit pkgsUnstable whichkey;};
        modules = [
          ./configuration.nix
          stylix.nixosModules.stylix

          # Home Manager integration
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-bak";

            # Pass both pkgsUnstable, vsx, and whichkey to HM modules
            home-manager.extraSpecialArgs = {inherit pkgsUnstable vsx whichkey;};

            home-manager.users.joebutler = import ./home.nix;
          }

          # Laptop-only deltas
          ./modules/hosts/laptop-nix.nix
        ];
      };

      # Desktop (auto-selected when hostname == "desktop-nix")
      desktop-nix = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit pkgsUnstable whichkey;};
        modules = [
          ./configuration.nix
          stylix.nixosModules.stylix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-bak";

            # Pass both pkgsUnstable, vsx, and whichkey to HM modules
            home-manager.extraSpecialArgs = {inherit pkgsUnstable vsx whichkey;};

            home-manager.users.joebutler = import ./home.nix;
          }

          # Desktop-only deltas
          ./modules/hosts/desktop-nix.nix
        ];
      };
    };

    # Standalone home-manager configurations
    homeConfigurations = {
      joebutler = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {inherit pkgsUnstable vsx whichkey;};
        modules = [
          ./home.nix
          stylix.homeModules.stylix
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
