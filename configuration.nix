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
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
    consoleMode = "max";
  };

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 2;
  boot.tmp.useTmpfs = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Boost time optimizations
  boot.consoleLogLevel = 0;
  boot.kernelParams = ["quiet"];
  boot.plymouth.enable = false;
  zramSwap.enable = true;

  # Power management - let auto-cpufreq handle scaling
  powerManagement = {
    enable = true;
    # cpuFreqGovernor removed to allow auto-cpufreq to manage scaling
  };

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;
  networking.firewall = {
    enable = true;
    # Tailscale now manages its own firewall via openFirewall = true
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Wayland portals - moved to services.nix
  # xdg.portal.enable = true;
  # xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];
  # xdg.portal.xdgOpenUsePortal = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";

  fonts = {
    fontconfig.enable = true;
    packages = with pkgs; [
      nerd-fonts.jetbrains-mono # Includes regular + Nerd Font functionality
      font-awesome
      noto-fonts-emoji
    ];
  };

  # Enable sound - moved to services.nix
  # services.pipewire = { enable = true; pulse.enable = true; };

  # Thunar needs these services for mounts and thumbnails
  services.gvfs.enable = true;
  services.tumbler.enable = true;

  # Polkit (auth prompts) - moved to services.nix
  # security.polkit.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.joebutler = {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    extraGroups = ["networkmanager" "wheel"]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  programs.hyprland.enable = true;
  hardware.graphics.enable = true;
  programs.zsh.enable = true;

  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
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
