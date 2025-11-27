{
  perSystem = {
    pkgs,
    inputs',
    ...
  }: {
    devShells.default = pkgs.mkShell {
      buildInputs = [
        inputs'.ai-utilities.packages.openskills
        pkgs.git
        pkgs.sops
      ];
    };
  };
}
