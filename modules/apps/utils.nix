{
  pkgs,
  pkgsUnstable,
  ...
}: {
  home.packages =
    (with pkgsUnstable; [
      hyprshot
      hyprland-protocols
      wl-clipboard
      xdg-utils
      wayland-utils
      uwsm
    ])
    ++ (with pkgs; [
      bluetui
      blueman
      localsend
      viewnior
      xfce.mousepad
      gvfs
      papirus-icon-theme
      hicolor-icon-theme
      desktop-file-utils

      # Fonts
      nerd-fonts.jetbrains-mono
      font-awesome
      noto-fonts-color-emoji

      qmk
      imagemagick
      brightnessctl

      # Media / Streaming
      ffmpeg
      grim
      slurp
      scrcpy

      networkmanagerapplet
      alsa-utils
      pavucontrol
    ]);

  xdg.dataFile."gtksourceview-4/language-specs/nix.lang".source = ./nix.lang;
  xdg.dataFile."gtksourceview-3.0/language-specs/nix.lang".source = ./nix.lang;
}
