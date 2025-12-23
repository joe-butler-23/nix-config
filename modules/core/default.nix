{
  pkgs,
  user,
  ...
}: {
  # ========================================
  # BOOT
  # ========================================
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
    consoleMode = "max";
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 2;
  boot.tmp.useTmpfs = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelModules = ["uinput"];
  boot.consoleLogLevel = 0;
  boot.kernelParams = ["quiet"];
  boot.plymouth.enable = true;
  zramSwap.enable = true;

  # ========================================
  # SYSTEM & NIX
  # ========================================
  time.timeZone = "Europe/London";
  i18n.defaultLocale = "en_GB.UTF-8";

  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  nix.optimise.automatic = true;
  nix.optimise.dates = ["03:45"];

  # nixd LSP configuration
  nix.nixPath = ["nixpkgs=${pkgs.path}"];

  programs.nix-ld.enable = true;

  system.autoUpgrade = {
    enable = true;
    flake = "github:joe-butler-23/nix-config";
    flags = ["-L"];
    dates = "04:00";
    randomizedDelaySec = "45min";
    allowReboot = false;
  };

  system.stateVersion = "25.05";

  imports = [
    ./hardware.nix
    ./sys-apps.nix
    ./environment.nix
    ./user-dirs.nix
    ./sops.nix
  ];

  # ========================================
  # USERS
  # ========================================
  users.users.${user} = {
    isNormalUser = true;
    createHome = true;
    extraGroups = ["networkmanager" "wheel" "input" "docker"];
    shell = pkgs.zsh;
  };
  # Enable Zsh at system level (required for user shell)
  programs.zsh.enable = true;

  # ========================================
  # SECURITY
  # ========================================
  security.polkit.enable = true;

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      AllowTcpForwarding = false;
      X11Forwarding = false;
      LogLevel = "VERBOSE";
      MaxAuthTries = 3;
      LoginGraceTime = 20;
    };
    extraConfig = ''
      AllowUsers ${user}
    '';
  };

  # SOPS Secrets - configured in ./sops.nix

  # Uinput for Espanso
  services.udev.extraRules = ''
    KERNEL=="uinput", MODE="0660", GROUP="input", OPTIONS+="static_node=uinput"
  '';

  # ========================================
  # NETWORKING
  # ========================================
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;
  networking.nameservers = [ "127.0.0.1" "::1" ];
  networking.networkmanager.dns = "none";
  
  # AdGuard Home (Local DNS & Dashboard)
  services.adguardhome = {
    enable = true;
    openFirewall = true;
    port = 3000;
    settings = {
      schema_version = 20;
    };
  };

  services.tailscale = {
    enable = true;
    openFirewall = true;
  };

  # ========================================
  # HARDWARE SENSORS
  # ========================================
  # NOTE: You might need to run `sensors-detect` and add suggested kernel modules here.

  # ========================================
  # SERVICES
  # ========================================
  # Audio
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  # Bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.settings = {
    General = {
      Enable = "Source,Sink,Media,Socket,HID";
      ControllerMode = "dual"; # Allow BR/EDR and LE
      FastConnectable = true; # Improve connection stability
    };
  };
  services.blueman.enable = true;

  # Better Bluetooth headset support (HFP/HSP profiles)
  services.pipewire.wireplumber.configPackages = [
    (pkgs.writeTextDir "share/wireplumber/bluetooth.lua.d/51-bluez-config.lua" ''
      bluez_monitor.properties = {
        ["bluez5.headset-roles"] = "[ hsp_hs hsp_ag hfp_hf hfp_ag ]"
      }
    '')
  ];

  # Windows network discovery
  services.samba-wsdd = {
    enable = true;
    openFirewall = true;
  };

  # Perf & Power
  services.irqbalance.enable = true;
  services.fstrim.enable = true;
  services.fwupd.enable = true;
  powerManagement.enable = true;

  # Docker
  virtualisation.docker.enable = true;
}
