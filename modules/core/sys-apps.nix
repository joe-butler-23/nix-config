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
}
