{pkgs}: let
  om = import ./om-lib.nix {inherit pkgs;};
in
  om.mkOmScript {
    name = "om-ctx-add";
    op = "add";
    category = "context";
  }
