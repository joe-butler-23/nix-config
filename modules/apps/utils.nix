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
      gtksourceview4
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
}
