{pkgs, ...}: {
  # Symlink Emacs configuration directory
  # (Keep for now - config files are already plain elisp)
  home.file.".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
  };

  # Install Emacs binary only - packages managed by straight.el
  home.packages = [
    pkgs.emacs-pgtk # Wayland-native Emacs
  ];

  # Suppress native compilation warnings
  home.sessionVariables = {
    EMACS_NATIVE_COMP_ASYNC_REPORT_WARNINGS_ERRORS = "nil";
  };
}
