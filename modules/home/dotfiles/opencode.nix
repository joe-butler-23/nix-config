{ pkgs, lib, config, ... }: {
  programs.opencode = {
    enable = true;
    settings = {
      mcp = builtins.fromJSON (builtins.readFile "${config.home.homeDirectory}/.config/opencode/mcp.json");
    };
  };
}
