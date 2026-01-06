{
  pkgs,
  pkgsUnstable,
  user,
  whichkey,
  anki-forge,
  ...
}: let
  # R with required packages
  R-with-packages = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      languageserver
      tidyverse
      rmarkdown
      knitr
    ];
  };

  # Anki with addons
  anki-with-addons = pkgsUnstable.anki.withAddons [
    (pkgsUnstable.ankiAddons.anki-connect.withConfig {
      config = {
        webCorsOriginList = [
          "http://localhost"
          "http://localhost:5173"
          "http://127.0.0.1:5173"
          "file://"
          "null"
        ];
      };
    })
  ];

  # Smart Anki Launcher
  ankiSmart = pkgs.writeShellScriptBin "anki-smart" ''
    if pgrep -x "anki" > /dev/null; then
      echo "Anki is already running. Check your scratchpad (Special Workspace)."
      ${pkgs.libnotify}/bin/notify-send -u normal "Anki is already running" "Check your scratchpad (Special Workspace)"
    else
      anki "$@"
    fi
  '';

  # Anki Forge Launcher
  ankiForgeLauncher = pkgs.writeShellScriptBin "anki-forge-launcher" ''
    if ! pgrep -x "anki" > /dev/null; then
      echo "Launching Anki in scratchpad..."
      hyprctl dispatch exec "[workspace special:magic] anki"
    else
      echo "Anki is already running."
      ${pkgs.libnotify}/bin/notify-send -u low "Anki already active" "Launching Anki Card Forge..."
    fi
    anki-card-forge
  '';
in {
  environment.systemPackages = [
    # Core Utilities
    pkgs.git
    pkgs.jujutsu
    pkgs.vim
    pkgs.curl
    pkgs.sqlite
    pkgs.jq

    # System Administration
    pkgs.sbctl
    pkgs.snapper
    pkgs.zram-generator
    pkgs.pre-commit
    pkgs.deadnix
    pkgs.statix
    pkgs.nixd
    pkgs.lm_sensors

    # Terminal & Desktop
    pkgs.kitty # Config managed by chezmoi
    pkgs.zathura # Config managed by chezmoi
    pkgs.waybar # Config managed by chezmoi
    pkgs.wlogout # Config managed by chezmoi
    pkgs.hyprland # Config managed by chezmoi
    pkgs.hypridle # Config managed by chezmoi
    pkgs.hyprlock # Config managed by chezmoi
    pkgs.rofi # Config managed by chezmoi
    pkgs.swaybg # Wallpaper daemon
    pkgs.mako # Config managed by chezmoi

    # Shell
    pkgs.zsh # Config managed by chezmoi
    pkgs.zsh-powerlevel10k
    pkgs.zsh-syntax-highlighting
    pkgs.zsh-autosuggestions
    pkgs.zsh-history-substring-search

    # File Manager
    pkgs.yazi # Config managed by chezmoi

    # Shell & CLI Tools
    pkgs.bat
    pkgs.bind
    pkgs.chezmoi
    pkgs.clipse
    pkgs.fd
    pkgs.fzf
    pkgs.gum
    pkgs.htop
    pkgs.inotify-tools
    pkgs.jq
    pkgs.lazygit
    pkgs.nix-search-tv
    pkgs.ripgrep
    pkgs.rclone
    pkgs.tmux
    pkgs.trash-cli
    pkgs.unzip
    pkgs.wget
    pkgs.zoxide
    pkgs.iwd
    pkgs.sshpass

    # Desktop Utilities
    pkgsUnstable.hyprshot
    pkgsUnstable.hyprland-protocols
    pkgsUnstable.wl-clipboard
    pkgs.kdotool
    pkgsUnstable.xdg-utils
    pkgsUnstable.wayland-utils
    pkgsUnstable.uwsm
    pkgs.bluetui
    pkgs.blueman
    pkgs.localsend
    pkgs.nwg-look
    pkgs.viewnior
    pkgs.xfce.mousepad
    pkgs.gtksourceview4
    pkgs.gvfs
    pkgs.desktop-file-utils

    # Theming & Cursors
    pkgs.capitaine-cursors
    pkgs.papirus-icon-theme
    pkgs.hicolor-icon-theme
    pkgs.qmk
    pkgs.sops
    pkgs.imagemagick
    pkgs.brightnessctl
    pkgs.upower
    pkgs.espanso-wayland

    # Media / Streaming
    pkgs.ffmpeg
    pkgs.grim
    pkgs.slurp
    pkgs.scrcpy
    pkgs.networkmanagerapplet
    pkgs.alsa-utils
    pkgs.pavucontrol
    pkgs.pulseaudio

    # Development Tools
    pkgsUnstable.app2unit
    pkgs.go
    pkgs.rustup
    pkgs.uv
    pkgs.scdoc
    pkgs.tectonic
    pkgs.texlive.combined.scheme-full
    pkgs.pandoc
    R-with-packages
    pkgs.nodejs_latest

    # Custom AI Tools (from overlay)
    pkgs.opencode
    pkgs.gemini
    pkgs.claude
    pkgs.codex
    pkgs.byterover
    pkgs.openmemory-js

    # OpenMemory CLI Wrappers
    (import ../scripts/om-add.nix { inherit pkgs; })
    (import ../scripts/om-query.nix { inherit pkgs; })
    (import ../scripts/om-ctx-add.nix { inherit pkgs; })
    (import ../scripts/om-ctx-query.nix { inherit pkgs; })
    (import ../scripts/om-pattern-add.nix { inherit pkgs; })
    (import ../scripts/om-pattern-query.nix { inherit pkgs; })
    (import ../scripts/om-changelog-add.nix { inherit pkgs; })
    (import ../scripts/om-changelog-query.nix { inherit pkgs; })
    (import ../scripts/om-list.nix { inherit pkgs; })
    (import ../scripts/om-stats.nix { inherit pkgs; })
    (import ../scripts/om-delete.nix { inherit pkgs; })

    # Applications
    pkgs.gnumeric
    pkgs.obsidian
    pkgs.zotero
    pkgs.vivaldi
    pkgs.vscodium

    # Editors
    pkgs.emacs-pgtk
    pkgs.zed-editor

    # Learning Tools
    anki-with-addons
    anki-forge.packages.x86_64-linux.default
    ankiSmart
    ankiForgeLauncher
    pkgs.libnotify

    # Desktop Environment
    whichkey.packages.x86_64-linux.wlr-which-key

    # Custom Scripts
    (import ../scripts/file-launcher.nix {inherit pkgs;})
    (import ../scripts/recent-files-launcher.nix {inherit pkgs;})
    (import ../scripts/copy-prompt.nix {inherit pkgs user;})
    (import ../scripts/directory-finder.nix {inherit pkgs;})
    (import ../scripts/study-focus.nix {inherit pkgs;})
    (import ../scripts/rofi-quick-capture.nix {inherit pkgs;})
    (import ../scripts/maintenance/weekly-review.nix {inherit pkgs;})
    (import ../scripts/maintenance/file-review.nix {inherit pkgs;})
    (import ../scripts/maintenance/system-maintenance.nix {inherit pkgs;})
    (import ../scripts/maintenance/weekly-metrics.nix {inherit pkgs;})
  ];

  fonts = {
    packages = with pkgs; [
      nerd-fonts.jetbrains-mono
      font-awesome
      ibm-plex
      noto-fonts
      noto-fonts-color-emoji
      noto-fonts-cjk-sans
      dejavu_fonts
    ];
    fontconfig = {
      enable = true;
      antialias = true;
      hinting = {
        enable = true;
        autohint = false;
        style = "slight";
      };
      subpixel = {
        rgba = "rgb";
        lcdfilter = "light";
      };
      defaultFonts = {
        monospace = ["JetBrainsMono Nerd Font"];
        sansSerif = ["DejaVu Sans" "Noto Sans"];
        serif = ["Noto Serif"];
      };
    };
  };

  # thunar dotfiles in modules/apps/thunar.nix
  programs.thunar.enable = true;
  programs.xfconf.enable = true; # Required for settings storage

  # Thunar Dependencies (Volume management & Thumbnails)
  services.gvfs.enable = true;
  services.tumbler.enable = true;

  # PAM configuration for Hyprlock
  security.pam.services.hyprlock = {};
}
