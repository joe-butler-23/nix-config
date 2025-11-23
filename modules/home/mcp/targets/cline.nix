# modules/home/mcp/targets/cline.nix
{ config, pkgs, lib, ... }:

{
  services.mcp.targets = {
    cline = {
      directory = ".config/VSCodium/User/globalStorage/saoudrizwan.cline-nightly/settings";
      fileName = "cline_mcp_settings.json";
    };
  };
}
