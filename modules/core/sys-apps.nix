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
    hyprland # Config managed by chezmoi
    hypridle # Config managed by chezmoi
    hyprlock # Config managed by chezmoi
    rofi # Config managed by chezmoi
    swaybg # Wallpaper daemon
    mako # Config managed by chezmoi

    # Shell
    zsh # Config managed by chezmoi
    zsh-powerlevel10k
    zsh-syntax-highlighting
    zsh-autosuggestions
    zsh-history-substring-search

    # File Manager
    yazi # Config managed by chezmoi
  ];

  # thunar dotfiles in modules/apps/thunar.nix
  programs.thunar.enable = true;
  programs.xfconf.enable = true; # Required for settings storage

  # Thunar Dependencies (Volume management & Thumbnails)
  services.gvfs.enable = true;
  services.tumbler.enable = true;
}
