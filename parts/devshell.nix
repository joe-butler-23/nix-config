{
  perSystem = {pkgs, ...}: {
    devShells.default = pkgs.mkShell {
      buildInputs = [
        pkgs.git
        pkgs.sops
      ];
    };
  };
}
