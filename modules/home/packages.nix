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
      ## Home Manage
      home-manager

      ## Wayland / Hyprland
      hypridle
      hyprlock
      hyprshot
      hyprpaper
      hyprland-protocols
      # rofi-wayland removed - let Stylix handle rofi package
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
      clipse
      fd
      fzf
      gemini-cli
      git
      jq
      lazygit
      nix-search-tv
      ripgrep
      tmux
      unzip
      wget
      # starship moved to system packages to fix timing issue
      # zoxide moved to system packages to fix timing issue

      ## Networking / internet
      brave
      iwd
      sshpass

      ## Programming / build
      go
      opencode
      rustup
      scdoc
      uv

      ## Sound / power
      alsa-utils
      pavucontrol

      ## Apps
      anki-bin
      espanso-wayland
      gnumeric
      obsidian
      zotero
      zathura
    ]);
}
