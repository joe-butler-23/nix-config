_: {
  # Thunar package installation and services (gvfs, tumbler) are in modules/core/sys-apps.nix

  # Thunar preferences via Xfconf (Home Manager)
  xfconf.settings = {
    thunar = {
      "last-show-hidden" = true;
      "misc-directory-specific-settings" = true;
    };
  };
}
