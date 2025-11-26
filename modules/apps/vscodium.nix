# modules/home/vscodium.nix
{
  pkgs,
  vsx,
  ...
}: {
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    mutableExtensionsDir = false;

    profiles.default = {
      extensions = [
        # Cline Nightly from generated marketplace set
        vsx.saoudrizwan.cline-nightly
        # Tokyo Night theme from nixpkgs
        pkgs.vscode-extensions.enkia.tokyo-night
        vsx.reditorsupport.r
        vsx.reditorsupport."r-syntax"
      ];

      userSettings = {
        "telemetry.telemetryLevel" = "off";
        "update.mode" = "none";
        "workbench.colorTheme" = "Tokyo Night Light";
      };
    };
  };
}
