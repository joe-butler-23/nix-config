# modules/home/mcp/targets/cline.nix
{ config, pkgs, lib, ... }:

{
  services.mcp.targets.cline = {
    directory = "${config.home.homeDirectory}/VSCodium/User/globalStorage/saoudrizwan.claude-dev/settings";
    fileName  = "cline_mcp_settings.json";
  };
}
