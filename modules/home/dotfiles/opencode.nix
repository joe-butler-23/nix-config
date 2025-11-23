# modules/home/dotfiles/opencode.nix
{ config, lib, ... }:

let
  # Get the mcp configuration from core.nix
  mcpCfg = config.services.mcp;

  # Transform the server definitions from our format to the format opencode.json expects
  opencodeMcpConfig = lib.mapAttrs (serverName: serverDef: {
    type = "local";
    command = [ serverDef.command ] ++ serverDef.args;
    environment = serverDef.env;
  }) mcpCfg.servers;

  # Define the opencode.json settings.
  # Other settings can be added here as needed.
  opencodeSettings = {
    mcp = opencodeMcpConfig;
  };
in
{
  # This home-manager option creates the opencode.json file from the attribute set above.
  # It converts the Nix code into a JSON string.
  home.file.".config/opencode/opencode.json".text =
    builtins.toJSON opencodeSettings;
}
