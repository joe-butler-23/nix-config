# modules/packages/dev.nix
{
  pkgsUnstable,
  ...
}: {
  environment.systemPackages = with pkgs; [
    git
    lazygit

    # Programming / build
    go
    uv
    rustup
    scdoc
  ]
  ++ [
    # Unstable packages
    pkgsUnstable.app2unit
  ];
}
