# modules/home/packages.nix
{
  pkgs,
  pkgsUnstable,
  ...
}: {
  #### User packages
  home.packages =
    [
      pkgsUnstable.app2unit
    ]
    ++ (with pkgs; [
      ## Wayland / Hyprland
      hypridle
      hyprlock
      hyprshot
      hyprpaper
      hyprland-protocols
      rofi-wayland
      mako
      waybar
      wl-clipboard
      xdg-desktop-portal-hyprland
      xdg-utils
      wayland-utils
      uwsm
      wlogout

      ## Core desktop
      foot
      capitaine-cursors
      bluetui
      blueman
      localsend
      viewnior
      xfce.mousepad
      xfce.thunar
      gvfs
      papirus-icon-theme
      hicolor-icon-theme
      desktop-file-utils
      nerd-fonts.jetbrains-mono
      font-awesome
      noto-fonts-emoji
      yazi
      qmk
      imagemagick
      brightnessctl

      ## Media / screenshots / streaming
      ffmpeg
      grim
      slurp
      scrcpy

      ## Shell / CLI tools
      bat
      ripgrep
      fd
      fzf
      git
      lazygit
      unzip
      wget
      zoxide
      starship
      zsh-autosuggestions
      zsh-history-substring-search
      zsh-syntax-highlighting

      ## Networking / internet
      brave
      iwd
      sshpass

      ## Programming / build
      go
      uv
      rustup
      scdoc

      ## Sound / power
      alsa-utils
      pavucontrol

      ## Apps
      anki-bin
      espanso-wayland
      obsidian
      vscodium
      zotero
      zathura
    ]);
}
