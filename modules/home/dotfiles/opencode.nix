{
  programs.opencode = {
    enable = true;
    settings = {
      mcp = builtins.fromJSON (builtins.readFile ./mcp.json);
    };
  };
}
