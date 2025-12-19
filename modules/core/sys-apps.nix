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
    pre-commit
    deadnix
    statix
    nixd
    lm_sensors

    # Terminal & Desktop
    kitty # Config managed by chezmoi
    zathura # Config managed by chezmoi
    waybar # Config managed by chezmoi

    # Temporary: mako notification daemon (should be in home.packages)
    mako
  ];

  # thunar dotfiles in modules/apps/thunar.nix
  programs.thunar.enable = true;
  programs.xfconf.enable = true; # Required for settings storage

  # Thunar Dependencies (Volume management & Thumbnails)
  services.gvfs.enable = true;
  services.tumbler.enable = true;
}
