{inputs, ...}: let
  system = "x86_64-linux";
  user = "joebutler";

  # Import overlays
  overlays = [
    inputs.emacs-overlay.overlays.default
    (import ../modules/overlays/default.nix {inherit inputs;})
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

  # Generated extensions set

  # ========================================
  # NIXOS SYSTEM CONFIGURATION (mkSystem)
  # Builds the full operating system
  # ========================================
  mkSystem = hostName:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit pkgsUnstable user;
        inherit (inputs) whichkey anki-forge hyprdynamicmonitors;
      };
      modules = [
        ../modules/core
        ../modules/services
        ../modules/desktop

        # System Modules (Enable System-wide Features)
        inputs.sops-nix.nixosModules.sops
        inputs.ai-utilities.nixosModules.default
        inputs.hyprdynamicmonitors.nixosModules.default

        # Use readOnlyPkgs to properly inject our custom pkgs with overlays
        {nixpkgs.pkgs = pkgs;}

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
