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

    # Anki-forge from GitHub repository
    anki-forge.url = "git+file:///home/joebutler/development/anki-forge-app";
  };

  outputs = {
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    stylix,
    treefmt-nix,
    nix-vscode-extensions,
    whichkey,
    anki-forge,
    ...
  }: let
    system = "x86_64-linux";
    pkgsUnstable = import nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;
    };
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    # Generated extensions set (kept up-to-date by upstream CI)
    vsx = nix-vscode-extensions.extensions.${system}.vscode-marketplace;

    # Helper function to generate a NixOS system configuration
    mkSystem = hostName:
      nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit pkgsUnstable whichkey anki-forge;};
        modules = [
          ./configuration.nix
          stylix.nixosModules.stylix

          # Home Manager integration
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-bak";

            # Pass special arguments to Home Manager modules
            home-manager.extraSpecialArgs = {inherit pkgsUnstable vsx whichkey anki-forge;};

            home-manager.users.joebutler = import ./home.nix;
          }

          # Host-specific configuration
          ./modules/hosts/${hostName}.nix
        ];
      };
  in {
    nixosConfigurations = {
      laptop-nix = mkSystem "laptop-nix";
      desktop-nix = mkSystem "desktop-nix";
    };

    # Standalone home-manager configurations
    homeConfigurations = {
      joebutler = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {inherit pkgsUnstable vsx whichkey anki-forge;};
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
