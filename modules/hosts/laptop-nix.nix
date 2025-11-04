{ lib, ... }:
{
  # Host name
  networking.hostName = "laptop-nix";

  # If you want these here (optional; shared config can hold them instead):
  # networking.networkmanager.enable = true;
  # networking.firewall.enable = true;
}
