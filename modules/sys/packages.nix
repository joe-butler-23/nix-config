# modules/sys/packages.nix
{pkgs, ...}: {
  #### System-wide packages
  environment.systemPackages = with pkgs; [
    ## Core system utilities
    sbctl
    snapper
    zram-generator

    ## Shell tools (moved from home-manager to fix timing)
    zsh
  ];
}
