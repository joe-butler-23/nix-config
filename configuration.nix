{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ./modules/packages.nix
    ./modules/services.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Wayland portals so file pickers, screenshare, xdg-open, etc. work
  xdg.portal.enable = true;
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];
  xdg.portal.xdgOpenUsePortal = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";

  fonts = {
    fontconfig.enable = true;
    packages = with pkgs; [
      jetbrains-mono
      nerd-fonts.jetbrains-mono
      font-awesome
      noto-fonts-emoji
    ];
  };

  # Enable sound.
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Thunar needs these services for mounts and thumbnails
  services.gvfs.enable = true;
  services.tumbler.enable = true;

  # Polkit (auth prompts)
  security.polkit.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.me = {
    isNormalUser = true;
    createHome = true;
    extraGroups = ["networkmanager" "wheel"]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  programs.hyprland.enable = true;
  programs.zsh.enable = true;

  nix.settings.experimental-features = ["nix-command" "flakes"];
  nixpkgs.config.allowUnfree = true;

  # Helpful Wayland env tweaks for apps
  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
  };

  environment.variables = {
    XCURSOR_THEME = "capitaine-cursors";
    XCURSOR_SIZE = "24";
  };

  system.stateVersion = "25.05";
}
