# System-level SOPS configuration
{user, ...}: {
  sops = {
    age.keyFile = "/home/${user}/nix-config/secrets/sops.agekey";
    defaultSopsFile = ../../secrets/secrets.yaml;
    # CONTEXT7_API_KEY: handled by home-manager sops for AI tools
  };
}
