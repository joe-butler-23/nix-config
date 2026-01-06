{pkgs}: let
  om = import ./om-lib.nix {inherit pkgs;};
in
  om.mkOmScript {
    name = "om-query";
    op = "query";
  }
