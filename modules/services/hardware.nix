# modules/services/hardware.nix
{
  ...
}: {
  #### Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  #### Power management
  services.tlp.enable = true;
}
