# System-level SOPS configuration
{user, ...}: {
  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;

    # Use SSH host key - available at boot before home directory is mounted
    age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];

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
