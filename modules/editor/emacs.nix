{
  pkgs,
  ...
}: {
  # Symlink Emacs configuration directory
  home.file.".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
  };

  programs.emacs = {
    enable = true;

    # Use emacs-pgtk for Wayland native support
    package = pkgs.emacs-pgtk;

    # Minimal package set for initial configuration
    extraPackages = epkgs:
      with epkgs; [
        # Performance
        gcmh # Garbage Collector Magic Hack

        # Evil mode (Vim emulation)
        evil
        evil-collection
        evil-surround

        # Discovery
        which-key

        # use-package is built-in on Emacs 29+, but we ensure it's available
        use-package
      ];
  };

  # Enable native compilation warnings suppression during build
  home.sessionVariables = {
    EMACS_NATIVE_COMP_ASYNC_REPORT_WARNINGS_ERRORS = "nil";
  };
}
