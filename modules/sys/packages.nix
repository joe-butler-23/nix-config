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
  ];
}
