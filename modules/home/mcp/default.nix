# $HOME/nix-config/modules/home/mcp/default.nix
{ ... }:

{
  imports = [
    ./core.nix
    # later: ./servers/context7.nix
    # later: ./targets/cline.nix
    # etc.
  ];
}
