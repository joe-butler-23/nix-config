# $HOME/nix-config/modules/home/mcp/default.nix
{...}: {
  imports = [
    ./core.nix
    ./servers/context7.nix
    ./targets/cline.nix
    ./settings.nix
  ];
}
