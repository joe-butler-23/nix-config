# modules/apps/vscodium.nix
{
  pkgs,
  vsx,
  ...
}: {
  environment.systemPackages = [
    (pkgs.vscode-with-extensions.override {
      vscode = pkgs.vscodium;
      vscodeExtensions = [
        # Cline Nightly (from generated marketplace set)
        vsx.saoudrizwan.cline-nightly
        # Kombai (newly added)
        vsx.kombai.kombai
        # Tokyo Night theme
        pkgs.vscode-extensions.enkia.tokyo-night
        # Nix support
        pkgs.vscode-extensions.bbenoist.nix
        # R support
        vsx.reditorsupport.r
        vsx.reditorsupport."r-syntax"
      ];
    })
  ];
}
