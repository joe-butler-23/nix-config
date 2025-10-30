# modules/packages/networking.nix
{
  ...
}: {
  environment.systemPackages = with pkgs; [
    # Networking / internet
    brave
    iwd
    sshpass
    tailscale
    syncthing
  ];
}
