# modules/sys/packages.nix
{pkgs, ...}: {
  #### System-wide packages
  environment.systemPackages = with pkgs; [
    ## Core system utilities
    sbctl
    snapper
    zram-generator

    ## Power management helpers
    auto-cpufreq

    ## Shell tools (moved from home-manager to fix timing)
    zsh
    starship
  ];
}
