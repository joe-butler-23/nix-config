{
  config,
  ...
}: {
  networking.hostName = "laptop-nix";

  assertions = [
    {
      assertion = config.networking.hostName == "laptop-nix";
      message = "Refusing to build: this module is only for host 'laptop-nix'.";
    }
  ];

  # Laptop-specific power management services
  services.thermald.enable = true;
  services.tlp.enable = true;

  # TLP configuration - prevent keyboard standby when plugged in
  services.tlp.settings = {
    # Disable USB autosuspend when on AC power (when keyboard is in use)
    USB_AUTOSUSPEND_ON_AC = 0;
    USB_AUTOSUSPEND_ON_BAT = 1;

    # Exclude input devices from USB autosuspend
    USB_EXCLUDE_INPUT = 1;

    # Exclude specific USB devices by vendor/product
    USB_DEVICE_BLACKLIST = "4653:0004"; # Corne keyboard vendor:product
  };

  # Note: Kanshi service is now system-level (modules/services/kanshi.nix)
  # Config is managed by chezmoi at ~/.config/kanshi/config
}
