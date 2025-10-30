# modules/core/system.nix
{
  ...
}: {
  # Polkit (auth prompts)
  security.polkit.enable = true;

  # Thunar needs these services for mounts and thumbnails
  services.gvfs.enable = true;
  services.tumbler.enable = true;

  fonts = {
    fontconfig.enable = true;
    packages = with pkgs; [
      jetbrains-mono
      nerd-fonts.jetbrains-mono
      font-awesome
      noto-fonts-emoji
    ];
  };
}
