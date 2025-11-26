{
  pkgs,
  pkgsUnstable,
  ...
}: let
  R-with-packages = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      languageserver
      tidyverse
      rmarkdown
      knitr
      httpgd
    ];
  };
in {
  home.packages =
    (with pkgsUnstable; [
      app2unit
    ])
    ++ (with pkgs; [
      go
      rustup
      uv

      # Latex / Docs
      scdoc
      tectonic
      texlive.combined.scheme-full
      pandoc

      # R Environment
      R-with-packages

      # Custom AI Tools
      opencode
      gemini
    ]);
}
