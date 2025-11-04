{ config, ... }:
{
  networking.hostName = "desktop-nix";

  assertions = [{
    assertion = config.networking.hostName == "desktop-nix";
    message = "Refusing to build: this module is only for host 'desktop-nix'.";
  }];
}
