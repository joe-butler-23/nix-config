# sops-nix Guide

This document provides a guide to setting up and using `sops-nix` for managing secrets in a NixOS and Home Manager environment. It is based on the official `sops-nix` repository and community best practices.

`sops-nix` provides a declarative, atomic, and reproducible way to manage secrets in NixOS and Home Manager configurations, allowing encrypted secrets to be committed to version control. It supports GPG and Age keys for decryption, with Age being recommended for its compatibility with SSH keys.

## 1. Initial Setup and Configuration

### 1.1. Flake Integration
Add `sops-nix` as an input in your `flake.nix`:
```nix
# flake.nix
{
  inputs = {
    # ... other inputs
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs"; # Ensure it uses your nixpkgs version
    };
  };
  outputs = { self, nixpkgs, sops-nix, ... }:
    {
    nixosConfigurations.yourhostname = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        sops-nix.nixosModules.sops # For NixOS
      ];
    };
    homeConfigurations.yourusername = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        ./home.nix
        sops-nix.homeManagerModules.sops # For Home Manager
      ];
    };
  };
}
```

### 1.2. `.sops.yaml` Configuration
Create a `.sops.yaml` file in the root of your configuration repository to define decryption keys and creation rules. This file specifies which keys can decrypt which secrets.
```yaml
# .sops.yaml
keys:
  - &admin_michael age10e9tt2qwq90y5hvl35dau0sm5cm4qvegtw2a70v7sz5fy99de42s9d5nkf # Your personal age public key
  - &server_batchn age1wnwfnrqhewjh39pmtyc8zhqw606znskt4h5p9s3pve4apd67gapqj6tr0k # Server's age public key
creation_rules:
  - path_regex: secrets/[^/]+\.(yaml|json|env|ini)$
    key_groups:
      - age:
          - *admin_michael
          - *server_batchn
```
The `path_regex` defines which files these rules apply to, and `key_groups` lists the Age or GPG keys required for decryption.

## 2. Generating and Using Age Keys

Age keys are preferred for their simplicity and ability to be derived from existing SSH keys.

### 2.1. Personal Age Key Generation
You can generate a new Age key or convert an existing SSH Ed25519 private key:
*   **Generate new:**
    ```bash
mkdir -p ~/.config/sops/age
age-keygen -o ~/.config/sops/age/keys.txt
    ```
*   **Convert SSH key:**
    ```bash
mkdir -p ~/.config/sops/age
nix run nixpkgs#ssh-to-age -- -private-key -i ~/.ssh/id_ed25519 -o ~/.config/sops/age/keys.txt
    ```
    To get the public key (recipient) from your generated `keys.txt`:
    ```bash
nix shell nixpkgs#age
age-keygen -y ~/.config/sops/age/keys.txt
    ```
    This public key (e.g., `age1...`) should be added to your `.sops.yaml` under `keys`.

### 2.2. Machine Age Key Generation
To allow your NixOS machine to decrypt secrets, derive an Age recipient from its SSH host key (e.g., `ssh_host_ed25519_key.pub`):
```bash
cat /etc/ssh/ssh_host_ed25519_key.pub | nix run nixpkgs#ssh-to-age
```
The output (e.g., `age1...`) is the machine's public key, which also needs to be added to your `.sops.yaml` under `keys`.

## 3. Creating and Editing Encrypted Secret Files

### 3.1. Creating/Editing Secrets
After configuring `.sops.yaml`, use the `sops` command to create or edit encrypted secret files. `sops` will automatically encrypt the file using the keys defined in `.sops.yaml` based on the file's path.
```bash
nix run nixpkgs#sops -- secrets/example.yaml
```
This command opens your default editor (`$EDITOR`). Enter your secrets in a supported format (YAML, JSON, INI, dotenv, or binary).

**Example `secrets/example.yaml` content:**
```yaml
api-key: hello world :)
myservice:
  my_subdir:
    my_secret: password123
```
After saving, `sops` encrypts the content, and the file will contain encrypted strings.

### 3.2. Updating Keys in Existing Secrets
If you add new keys to your `.sops.yaml` (e.g., for a new user or server), you need to re-encrypt existing secrets to include the new keys:
```bash
nix run nixpkgs#sops -- updatekeys secrets/example.yaml
```

## 4. Accessing Secrets within Nix Configuration

### 4.1. NixOS Configuration
In your `configuration.nix` (or relevant NixOS module), enable `sops-nix` and declare your secrets:
```nix
# configuration.nix
{
  config, pkgs, ...
}:
{
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ]; # Path to the machine's private SSH key for decryption
  sops.defaultSopsFile = ./secrets/example.yaml; # Default path to your sops file
  sops.secrets."api-key" = {}; # Declare the secret named "api-key"
  sops.secrets."myservice/my_subdir/my_secret" = {}; # Declare nested secret
}
```
After a `nixos-rebuild switch`, secrets will be available as files in `/run/secrets/`.
*   `cat /run/secrets/api-key` will output `hello world :)
*   `cat /run/secrets/myservice/my_subdir/my_secret` will output `password123`

### 4.2. Home Manager Configuration
In your `home.nix` (or relevant Home Manager module), configure `sops` for user-level secrets:
```nix
# home.nix
{
  config, pkgs, ...
}:
{
  sops = {
    age.keyFile = "/home/user/.config/sops/age/keys.txt"; # Path to your personal age private key
    # age.sshKeyPaths = [ "/home/user/.ssh/id_ed25519" ]; # Alternative: use SSH key
    defaultSopsFile = ./secrets.yaml;
    secrets.test = {}; # Declare a secret
  };
  # Ensure services that need secrets start after sops-nix
  systemd.user.services.my-user-service.unitConfig.After = [ "sops-nix.service" ];
}
```
Home Manager secrets are decrypted to `$XDG_RUNTIME_DIR/secrets.d/` and symlinked to `$HOME/.config/sops-nix/secrets/`.

### 4.3. Secret Permissions and Ownership
You can set `mode`, `owner`, and `group` for secrets:
```nix
sops.secrets.example-secret = {
  mode = "0440"; # Read-only for owner and group
  owner = config.users.users.nobody.name;
  group = config.users.users.nobody.group;
};
```

### 4.4. Secrets Needed for Users (NixOS only)
For secrets required before user creation (e.g., `hashedPasswordFile`), use `neededForUsers = true`. These secrets are decrypted to `/run/secrets-for-users`.
```nix
sops.secrets.my-password.neededForUsers = true;
users.users.mic92 = {
  isNormalUser = true;
  hashedPasswordFile = config.sops.secrets.my-password.path;
};
```

### 4.5. Templates for Configuration Files
To embed secrets directly into configuration files, use `sops.templates`:
```nix
sops.secrets.your-secret = {};
sops.templates."your-config-with-secrets.toml".content = ''
  password = "${config.sops.placeholder.your-secret}"
'';
sops.templates."your-config-with-secrets.toml".owner = "serviceuser";

systemd.services.myservice = {
  serviceConfig = {
    ExecStart = "${pkgs.myservice}/bin/myservice --config ${config.sops.templates."your-config-with-secrets.toml".path}";
    User = "serviceuser";
  };
};
```

## 5. Practical Examples

### 5.1. Command-line Flags (NixOS)
Wrap `ExecStart` in a `pkgs.writeShellScript` to `cat` secrets into command-line arguments:
```nix
systemd.services.fortuneserver = {
  serviceConfig = {
    ExecStart = pkgs.writeShellScript "fortuneserver-execstart" "''
      \"${pkgs.fortuneserver}/bin/fortuneserver\" \
      -securecookie_hash_key=\"$(cat ${config.sops.secrets.\"fortuneserver/securecookie_hash_key\".path})\" \
      -securecookie_block_key=\"$(cat ${config.sops.secrets.\"fortuneserver/securecookie_block_key\".path})\"
    '';
  };
};
```

### 5.2. Environment Variable Files (NixOS)
Store environment variables in a secret file and use `EnvironmentFile` in systemd:
```nix
# secrets/translate-fe/env (sops file)
DEEPL_AUTH_KEY=my-deepl-key

# configuration.nix
sops.secrets."translate-fe/env" = { owner = "translatefe"; restartUnits = [ "translate-fe.service" ]; };
systemd.services.translate-fe = {
  serviceConfig = {
    User = "translatefe";
    EnvironmentFile = [ config.sops.secrets."translate-fe/env".path ];
    ExecStart = "${translatefeExecstart}/bin/translate-fe";
  };
};
```

### 5.3. Systemd Credentials (NixOS)
Use `LoadCredential` for services with `DynamicUser` or when a dedicated user account isn't desired:
```nix
sops.secrets."alertmanager/smtp_pw" = { restartUnits = [ "alertmanager.service" ]; };
systemd.services.alertmanager.serviceConfig.LoadCredential = [ "smtp_pw:${config.sops.secrets."alertmanager/smtp_pw".path}" ];
services.prometheus.alertmanager = {
  configuration = {
    global = {
      smtp_auth_password_file = "/run/credentials/alertmanager.service/smtp_pw";
    };
  };
};
```
