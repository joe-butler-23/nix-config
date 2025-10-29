# modules/packages.nix
{ pkgs, pkgsUnstable, ... }:
{
  environment.systemPackages =
    [
      pkgsUnstable.app2unit
    ] ++
    (with pkgs; [
      ## Wayland / Hyprland
      hypridle
      hyprlock
      hyprpaper
      hyprland-protocols
      rofi-wayland
      swaynotificationcenter
      waybar
      wl-clipboard
      xdg-desktop-portal-hyprland
      xdg-desktop-portal-gtk
      xdg-utils
      wayland-utils
      uwsm
      wlogout

      ## Core desktop utils
      foot
      bluetui
      localsend
      viewnior
      xfce.mousepad
      xfce.thunar
      gvfs
      papirus-icon-theme
      hicolor-icon-theme
      desktop-file-utils

      ## Media / screenshots / streaming
      ffmpeg
      grim
      slurp
      scrcpy

      ## Shell / CLI tools
      bat
      fd
      fzf
      git
      less
      unzip
      wget
      zoxide
      starship

      ## Networking / internet
      brave
      iwd
      sshpass
      tailscale
      syncthing

      ## Security / auth / policy
      polkit-gnome
      sbctl

      ## Programming / build
      go
      uv
      rustup
      scdoc

      ## Fonts
      jetbrains-mono

      ## Sound / power
      alsa-utils
      pavucontrol

      ## Filesystems / maintenance
      snapper
      zram-generator

      ## Apps
      anki-bin
      espanso-wayland
      obsidian
      vscodium
      zotero
      zathura
    ]);
}
