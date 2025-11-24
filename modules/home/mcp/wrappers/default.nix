# modules/home/mcp/wrappers/default.nix
# This file automatically imports all other .nix files in this directory.
{
  imports =
    (map (f: ./${f})
      (builtins.filter (f: f != "default.nix" && builtins.match ".*\\.nix" f != null)
        (builtins.attrNames (builtins.readDir ./.)
        )
      )
    );
}
