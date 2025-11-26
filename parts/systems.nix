{inputs, ...}: let
  system = "x86_64-linux";
  user = "joebutler";

  # Import overlays
  overlays = [
    (import ../modules/apps/overlays/default.nix {inherit inputs;})
  ];

  pkgsUnstable = import inputs.nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
    inherit overlays;
  };
  pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
    inherit overlays;
  };

  inherit (inputs.nixpkgs) lib;

  # Generated extensions set
  vsx = inputs.nix-vscode-extensions.extensions.${system}.vscode-marketplace;

  # ========================================
  # NIXOS SYSTEM CONFIGURATION (mkSystem)
  # Builds the full operating system (Bootloader, Kernel, Services + User Home)
  # ========================================
  mkSystem = hostName:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit pkgsUnstable user;
        inherit (inputs) whichkey anki-forge;
      };
      modules = [
        ../modules/core

        # System Modules (Enable System-wide Features)
        inputs.stylix.nixosModules.stylix
        inputs.sops-nix.nixosModules.sops

        # Use readOnlyPkgs to properly inject our custom pkgs with overlays
        {nixpkgs.pkgs = pkgs;}

        # Home Manager Integration (NixOS Module)
        inputs.home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "backup";

          # Pass special arguments to Home Manager modules
          home-manager.extraSpecialArgs = {
            inherit pkgsUnstable vsx user;
            inherit (inputs) whichkey anki-forge;
          };

          # User Configuration ("Home")
          home-manager.users.${user} = {
            home.username = user;
            home.homeDirectory = "/home/${user}";
            home.stateVersion = "25.05";

            imports = [
              ../modules/shell
              ../modules/desktop
              ../modules/apps
              ../modules/scripts

              # User Modules (Enable User-level Features)
              inputs.sops-nix.homeManagerModules.sops
            ];
          };
        }

        # Host-specific configuration
        ../modules/hosts/${hostName}.nix
      ];
    };
in {
  flake = {
    nixosConfigurations = {
      laptop-nix = mkSystem "laptop-nix";
      desktop-nix = mkSystem "desktop-nix";
    };
  };
}
