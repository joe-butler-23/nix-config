{inputs, ...}: {
  perSystem = {
    system,
    ...
  }: {
    formatter = inputs.treefmt-nix.lib.mkWrapper inputs.nixpkgs.legacyPackages.${system} {
      # Use alejandra as the Nix formatter
      programs.alejandra.enable = true;

      # Enable statix linter
      programs.statix.enable = true;

      # Enable deadnix dead code detector
      programs.deadnix.enable = true;
    };
  };
}
