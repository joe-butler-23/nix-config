{config, ...}: {
  services.mcp.targets = {
    opencode = {
      directory = "${config.xdg.configHome}/opencode";
      fileName = "mcp.json";
      format = "opencode";
    };
  };
}
