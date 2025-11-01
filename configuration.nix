{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ./modules/sys/packages.nix
    ./modules/sys/services.nix
  ];

  #### Boot configuration
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
    consoleMode = "max";
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 2;
  boot.tmp.useTmpfs = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.consoleLogLevel = 0;
  boot.kernelParams = ["quiet"];
  boot.plymouth.enable = false;
  zramSwap.enable = true;

  #### Power management
  powerManagement = {
    enable = true;
    # cpuFreqGovernor removed to allow auto-cpufreq to manage scaling
  };

  #### Networking
  networking.hostName = "nixos";
  networking.networkmanager.enable = true;
  networking.firewall = {
    enable = true;
    # Tailscale now manages its own firewall via openFirewall = true
  };

  #### Locale and time
  time.timeZone = "Europe/London";
  i18n.defaultLocale = "en_GB.UTF-8";

  #### User account
  users.users.joebutler = {
    isNormalUser = true;
    createHome = true;
    extraGroups = ["networkmanager" "wheel"]; # Enable 'sudo' for the user.
    shell = pkgs.zsh;
  };

  #### Nix settings
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  nixpkgs.config.allowUnfree = true;

  #### System state version
  system.stateVersion = "25.05";
}
