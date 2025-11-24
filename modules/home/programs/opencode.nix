{
  config,
  lib,
  pkgs,
  ...
}: with lib; {
  options.programs.opencode = {
    enable = mkEnableOption "OpenCode program";

    settings = mkOption {
      type = types.attrs;
      default = {};
      description = "OpenCode settings";
    };
  };
}
