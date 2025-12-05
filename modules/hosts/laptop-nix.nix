{
  config,
  user,
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

  # Laptop-only Home-Manager config
  home-manager.users.${user} = {
    services.kanshi.enable = true;

    xdg.configFile."kanshi/config".text = ''
      profile undocked {
        output eDP-1 enable position 0,0 mode 1920x1080
        exec hyprctl reload
      }

      profile docked {
        output "Dell Inc. DELL S2721HSX 1991Q83" enable position 0,0 mode 1920x1080
        output eDP-1 disable
        exec hyprctl reload
      }
    '';
  };
}
