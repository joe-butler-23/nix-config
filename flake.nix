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

    # VS Code/VSCodium extensions (Open VSX + Marketplace)
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    nix-vscode-extensions.inputs.nixpkgs.follows = "nixpkgs";

    # Local whichkey development flake
    whichkey.url = "git+file:///home/joebutler/development/whichkey";

    # Anki-forge from GitHub repository
    anki-forge.url = "github:joe-butler-23/anki-card-forge";

    # SOPS Nix
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
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
    sops-nix,
    ...
  }: let
    system = "x86_64-linux";
    user = "joebutler";
    pkgsUnstable = import nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;
    };
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    inherit (nixpkgs) lib; # Define lib here

    # Generated extensions set (kept up-to-date by upstream CI)
    vsx = nix-vscode-extensions.extensions.${system}.vscode-marketplace;

    # Helper function to generate a NixOS system configuration
    mkSystem = hostName:
      nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit pkgsUnstable whichkey anki-forge user;};
        modules = [
          ./configuration.nix
          stylix.nixosModules.stylix
          sops-nix.nixosModules.sops

          # Home Manager integration
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-bak";

            # Pass special arguments to Home Manager modules
            home-manager.extraSpecialArgs = {inherit pkgsUnstable vsx whichkey anki-forge user;};

            home-manager.users.${user} = import ./home.nix;
          }

          # Host-specific configuration
          ./modules/hosts/${hostName}.nix
        ];
      };

    # Evaluate home-manager configuration to get homeDirectory
    homeConfig = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = {inherit pkgsUnstable vsx whichkey anki-forge user;};
      modules = [
        ./home.nix
        stylix.homeModules.stylix
        sops-nix.homeManagerModules.sops
      ];
    };
  in {
    nixosConfigurations = {
      laptop-nix = mkSystem "laptop-nix";
      desktop-nix = mkSystem "desktop-nix";
    };

    # Standalone home-manager configurations
    homeConfigurations = {
      "${user}" = homeConfig;
    };

    # formatter unchanged
    formatter.${system} =
      treefmt-nix.lib.mkWrapper
      nixpkgs.legacyPackages.${system}
      (import ./treefmt.nix);
  };
}
