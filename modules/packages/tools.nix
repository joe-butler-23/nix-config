# modules/packages/tools.nix
{
  ...
}: {
  environment.systemPackages = with pkgs; [
    # Media / screenshots / streaming
    ffmpeg
    grim
    slurp
    scrcpy

    # Security / auth / policy
    polkit
    sbctl

    # Sound / power
    alsa-utils
    pavucontrol

    # Filesystems / maintenance
    snapper
    zram-generator

    # Fonts (already in core system, but keeping for completeness)
    jetbrains-mono
  ];
}
