# modules/packages.nix
{
  pkgs,
  pkgsUnstable,
  ...
}: {
  environment.systemPackages =
    [
      pkgsUnstable.app2unit
      # pkgsUnstable.python313Packages.keymap-drawer - marked as broken
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
      # xdg-desktop-portal-gtk moved to services only
      xdg-utils
      wayland-utils
      uwsm
      wlogout

      ## Core desktop utils
      foot
      capitaine-cursors
      bluetui
      localsend
      viewnior
      xfce.mousepad
      xfce.thunar
      gvfs
      papirus-icon-theme
      hicolor-icon-theme
      desktop-file-utils
      yazi
      qmk
      imagemagick
      # python3.keymap-drawer - commented out due to version conflicts
      # Can be re-enabled if needed

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
      # tailscale and syncthing moved to services only

      ## Security / auth / policy
      sbctl
      # polkit moved to services only

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
      auto-cpufreq

      ## Apps
      anki-bin
      espanso-wayland
      obsidian
      vscodium
      zotero
      zathura
    ]);
}
