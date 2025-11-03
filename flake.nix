{
  description = "My NixOS Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      # Track the 25.05 HM release; follows your nixpkgs
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

    elephant.url = "github:abenz1267/elephant";

    walker = {
      url = "github:abenz1267/walker";
      inputs.elephant.follows = "elephant";
    };
  };

  outputs = {
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    stylix,
    treefmt-nix,
    walker,
    elephant,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    pkgsUnstable = import nixpkgs-unstable {inherit system;};
  in {
    # Optional: allows `home-manager --flake .#joebutler switch`
    homeConfigurations."joebutler" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      extraSpecialArgs = {inherit pkgsUnstable;};
      modules = [
        {
          nixpkgs.config.allowUnfree = true;
        }
        ./home.nix
        stylix.homeModules.stylix # Import Stylix Home Manager module
        walker.homeManagerModules.default # Import Walker Home Manager module
      ];
    };

    # Main NixOS system (used by `sudo nixos-rebuild switch --flake ...`)
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      inherit system;

      # Make pkgsUnstable available to your modules
      specialArgs = {inherit pkgsUnstable;};

      modules = [
        ./configuration.nix
        stylix.nixosModules.stylix
        {
          nix.settings = {
            extra-substituters = ["https://walker.cachix.org" "https://walker-git.cachix.org"];
            extra-trusted-public-keys = ["walker.cachix.org-1:fG8q+uAaMqhsMxWjwvk0IMb4mFPFLqHjuvfwQxE4oJM=" "walker-git.cachix.org-1:vmC0ocfPWh0S/vRAQGtChuiZBTAe4wiKDeyyXM0/7pM="];
          };
        }

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
