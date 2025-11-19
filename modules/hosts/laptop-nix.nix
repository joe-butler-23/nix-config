{config, pkgs, ...}: {
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
  powerManagement.powertop.enable = false;

  # TLP configuration - prevent keyboard standby when plugged in
  services.tlp.settings = {
    # Disable USB autosuspend when on AC power (when keyboard is in use)
    USB_AUTOSUSPEND_ON_AC = 0;
    USB_AUTOSUSPEND_ON_BAT = 1;

    # Exclude input devices from USB autosuspend
    USB_EXCLUDE_INPUT = 1;

    # Exclude specific USB devices by vendor/product (corrected device ID)
    USB_DEVICE_BLACKLIST = "4653:0004"; # Corne keyboard vendor:product (corrected)
  };

  # Udev rules to prevent USB autosuspend for input devices
  services.udev.extraRules = ''
    # Prevent USB autosuspend for all HID (Human Interface Device) devices
    ACTION=="add", SUBSYSTEM=="usb", ATTR{bInterfaceClass}=="03", ATTR{bInterfaceProtocol}=="01", TEST=="power/control", ATTR{power/control}="on"
    
    # Prevent USB autosuspend for specific Corne keyboard
    ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="4653", ATTR{idProduct}=="0004", TEST=="power/control", ATTR{power/control}="on"
  '';

  # Systemd service to ensure USB power settings are applied on boot
  systemd.services.usb-power-fix = {
    description = "Ensure USB input devices stay powered on";
    wantedBy = [ "multi-user.target" ];
    after = [ "tlp.service" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = [
        # Set all HID devices to always on
        "${pkgs.bash}/bin/bash -c 'for device in /sys/bus/usb/devices/*/power/control; do if grep -q \"03\" \"$(dirname $device)/../bInterfaceClass\" 2>/dev/null; then echo \"on\" > \"$device\"; fi; done'"
        # Set specific Corne keyboard to always on
        "${pkgs.bash}/bin/bash -c 'for device in /sys/bus/usb/devices/*/idVendor; do if [ \"$(cat \"$device\")\" = \"4653\" ] && [ \"$(cat \"$(dirname $device)/idProduct\")\" = \"0004\" ]; then echo \"on\" > \"$(dirname $device)/power/control\"; fi; done'"
      ];
    };
  };

  # Laptop-only Home-Manager config
  home-manager.users.joebutler = {
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

      profile "double docked" {
        output "Acer Technologies KA240Y 4129031E83W01" enable position 0,0 mode 1920x1080
        output "Dell Inc. DELL S2721HSX 1991Q83" enable position 1920,0 mode 1920x1080
        output eDP-1 disable
        exec hyprctl reload
      }
    '';
  };
}
