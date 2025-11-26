{pkgs, ...}: {
  home.packages = with pkgs; [
    bat
    clipse
    fd
    fzf
    git
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
