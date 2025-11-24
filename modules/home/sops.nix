# modules/home/sops.nix
{
  sops = {
    age.keyFile = "/home/joebutler/nix-config/secrets/sops.agekey";
    defaultSopsFile = ./secrets/secrets.yaml;
    secrets.CONTEXT7_API_KEY = {};
  };
}
