{pkgs, ...}: {
  home.packages = with pkgs; [
    bat
    chezmoi
    clipse
    fd
    fzf
    git
    gum
    htop
    jq
    lazygit
    nix-search-tv
    ripgrep
    tmux
    unzip
    wget
    zoxide
    iwd
    sshpass
  ];
}
