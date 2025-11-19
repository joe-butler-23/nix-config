# modules/home/packages.nix
{
  pkgs,
  pkgsUnstable,
  ...
}: let
  # Define a custom R environment with specific packages
  R-with-packages = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      languageserver
      tidyverse
      rmarkdown
      knitr
      httpgd
    ];
  };
in {
  #### User packages
		home.packages = with pkgsUnstable; [
			app2unit
			gemini-cli
			(anki.withAddons [
				(ankiAddons.anki-connect.withConfig {
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
			])

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
			home-manager
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
      noto-fonts-color-emoji
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
      git
      htop
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
      R-with-packages
      rustup
      scdoc
      tectonic
      texlive.combined.scheme-full
      pandoc
      uv

      ## Sound / power
      alsa-utils
      pavucontrol

      ## Apps
      gnumeric
      obsidian
      zotero
      zathura
    ];
}
