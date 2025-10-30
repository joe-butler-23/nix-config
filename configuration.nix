{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix

    # Core system configuration
    ./modules/core/system.nix

    # Service modules
    ./modules/services/desktop.nix
    ./modules/services/networking.nix
    ./modules/services/hardware.nix

    # Package modules
    ./modules/packages/core.nix
    ./modules/packages/dev.nix
    ./modules/packages/networking.nix
    ./modules/packages/desktop.nix
    ./modules/packages/apps.nix
    ./modules/packages/tools.nix
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

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";

  # Enable sound.
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.me = {
    isNormalUser = true;
    createHome = true;
    extraGroups = ["networkmanager" "wheel"]; # Enable ‘sudo’ for the user.
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
