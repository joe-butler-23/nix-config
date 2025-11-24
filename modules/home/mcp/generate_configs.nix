{pkgs, ...}:
pkgs.writeShellApplication {
  name = "generate-mcp-configs";
  runtimeInputs = [pkgs.python3 pkgs.jq];
  text = ''
    ${pkgs.python3}/bin/python3 ${./generate_configs.py}
  '';
}
