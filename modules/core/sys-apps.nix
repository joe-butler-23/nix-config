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

    # Shell
    zsh # Config managed by chezmoi
    zsh-powerlevel10k
    zsh-syntax-highlighting
    zsh-autosuggestions
    zsh-history-substring-search

    # File Manager
    yazi # Config managed by chezmoi

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
