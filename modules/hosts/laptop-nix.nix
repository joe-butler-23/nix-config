{
  config,
  ...
}: {
  imports = [];

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
  services.upower.enable = true;

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

  # HyprDynamicMonitors: Replacement for Kanshi
  services.hyprdynamicmonitors = {
    enable = true;

    extraFlags = ["--disable-power-events"];

    # Correct schema based on source code and examples

    config = ''

      [general]

      destination = "/home/joebutler/.config/hypr/monitors.conf"



      [profiles.docked]

      config_file = "/home/joebutler/.config/hypr/monitors-docked.conf"

      config_file_type = "static"

      [profiles.docked.conditions]

      [[profiles.docked.conditions.required_monitors]]

      name = "DP-5"

      [[profiles.docked.conditions.required_monitors]]

      name = "eDP-1"



      [profiles.undocked]

      config_file = "/home/joebutler/.config/hypr/monitors-undocked.conf"

      config_file_type = "static"

      [profiles.undocked.conditions]

      [[profiles.undocked.conditions.required_monitors]]

      name = "eDP-1"

    '';
  };
}
