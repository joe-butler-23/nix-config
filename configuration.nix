{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ./modules/sys/packages.nix
    ./modules/sys/services.nix
    ./modules/sys/stylix.nix
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
  boot.plymouth.enable = true;
  zramSwap.enable = true;

  #### Locale and time
  time.timeZone = "Europe/London";
  i18n.defaultLocale = "en_GB.UTF-8";

  #### Nix settings
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  nix.optimise.automatic = true;
  nix.optimise.dates = ["03:45"];

  system.autoUpgrade = {
    enable = true;
    flake = "github:joe-butler-23/nix-config";
    flags = [
      "-L" # print build logs
    ];
    dates = "04:00";
    randomizedDelaySec = "45min";
    allowReboot = false;
  };
  nixpkgs.config.allowUnfree = true;

  #### System state version
  system.stateVersion = "25.05";

  # SOPS configuration
  sops.enable = true;
  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.age.keyFile = "/home/joebutler/nix-config/secrets/sops.agekey";
}
