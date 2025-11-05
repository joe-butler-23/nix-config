{pkgs, ...}: {
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;

    # Keep extensions reproducible & installed by Nix
    mutableExtensionsDir = false;

    # Cline (official) from Open VSX via nixpkgs
    extensions = with pkgs.vscode-extensions; [
      saoudrizwan.claude-dev
    ];

    # Minimal sensible defaults (extend later)
    userSettings = {
      "telemetry.telemetryLevel" = "off";
      "update.mode" = "none";
    };
  };
}
