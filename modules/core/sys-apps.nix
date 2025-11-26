{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    # Core Utilities
    git
    vim
    curl

    # System Administration
    sbctl
    snapper
    zram-generator
  ];

  # ============================================================================
  # THUNAR FILE MANAGER (System Level)
  # User-specific configuration (actions, keybinds) is handled in:
  # modules/apps/thunar.nix
  # ============================================================================
  programs.thunar.enable = true;
  programs.xfconf.enable = true; # Required for settings storage

  # Thunar Dependencies (Volume management & Thumbnails)
  services.gvfs.enable = true;
  services.tumbler.enable = true;
}
