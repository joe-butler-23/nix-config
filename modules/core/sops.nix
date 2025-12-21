# System-level SOPS configuration
{user, ...}: {
  sops = {
    age.keyFile = "/home/${user}/nix-config/secrets/sops.agekey";
    defaultSopsFile = ../../secrets/secrets.yaml;

    secrets.CONTEXT7_API_KEY = {
      mode = "0440";
      owner = user;
    };
  };

  # Make CONTEXT7_API_KEY available as environment variable
  environment.variables = {
    CONTEXT7_API_KEY_PATH = "/run/secrets/CONTEXT7_API_KEY";
  };
}
