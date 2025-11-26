# modules/home/sops.nix
{user, ...}: {
  sops = {
    age.keyFile = "/home/${user}/nix-config/secrets/sops.agekey";
    defaultSopsFile = ../../secrets/secrets.yaml;
    secrets.CONTEXT7_API_KEY = {};
  };
}
