# Espanso Strategy & Knowledge Base

## AI Validation Process

This section is for AI agents to verify changes work correctly before presenting to user.

### Post-Change Validation (Run after applying config)
```bash
# 1. Rebuild home-manager
home-manager switch

# 2. Verify service restarted successfully
systemctl --user is-active espanso

# 3. Check for errors in logs
journalctl --user -u espanso --since "1 minute ago" --no-pager

# 4. Verify symlinks were created by Home Manager
ls -l ~/.config/espanso/config/default.yml
ls -l ~/.config/espanso/match/base.yml
ls -l ~/.config/espanso/match/secrets.yml  # Should point to sops-nix managed file

# 5. Verify sops secret is decrypted
test -f ~/.config/sops-nix/secrets/secrets.yml && echo "✓ Secrets decrypted" || echo "✗ Secrets missing"
```

### User Testing Instructions
After applying changes, inform user to test new triggers:
- Type the new trigger in any text field
- Verify it expands to expected content
- Check that secrets expand without showing encrypted content

## Core Principles

### 1. Documentation First
**MANDATE:** All configuration MUST follow official Espanso documentation (https://espanso.org/docs/).
- Never guess at configuration options
- Always reference official docs when adding new features
- Document any deviations from standard config due to NixOS/Home Manager

### 2. Sops Integration
**MANDATE:** All sensitive data MUST use sops-nix.
- API keys, email addresses, personal info go in `secrets/secrets.yaml` under `espanso_matches`
- Secrets are automatically linked to `~/.config/espanso/match/secrets.yml` via Home Manager
- Never commit plain-text secrets to the repository

### 3. Structure
```
modules/apps/
├── espanso.nix                   # Main configuration
└── espanso.md                    # This strategy guide

secrets/secrets.yaml              # Encrypted secrets (includes espanso_matches key)
                                  # → Auto-linked to ~/.config/espanso/match/secrets.yml
```

## How to Add Sops Secrets for Espanso

### Step 1: Edit the encrypted secrets file
```bash
sops secrets/secrets.yaml
```

If `sops` fails with `no identity matched any of the recipients`, use the repo key explicitly:
```bash
SOPS_AGE_KEY_FILE="/home/$USER/nix-config/secrets/sops.agekey" sops "/home/$USER/nix-config/secrets/secrets.yaml"
```

### Step 2: Add espanso matches under the `espanso_matches` key
The value should be valid YAML for an Espanso match file:
```yaml
espanso_matches: |
  matches:
    - trigger: ":myemail"
      replace: "my.secret.email@example.com"

    - trigger: ":apikey"
      replace: "sk-proj-abc123..."

    - trigger: ":home"
      replace: "123 Private Street, City"
```

### Step 3: Save and exit
Sops will encrypt the file automatically.

### Step 4: The secret is automatically available
Home Manager configuration in `espanso.nix` (lines 60-63) already sets up:
- Decryption of the secret
- Symlinking to `~/.config/espanso/match/secrets.yml`
- Espanso service dependency on sops-nix.service

### Step 5: Apply changes
```bash
home-manager switch
```

### Important Notes
- The `espanso_matches` value must be valid YAML (use `|` for multiline)
- Espanso will load all `.yml` files in the match directory automatically
- Secrets are decrypted at activation time and placed in `~/.config/sops-nix/secrets/`
- The symlink ensures Espanso can read them without exposing them in the nix store

## Current Configuration Audit (2025-12-15)

### Installation Status
- **Version:** 2.3.0 (espanso-wayland)
- **Service Status:** Active and running
- **Backend:** Inject (for Wayland compatibility)
- **Keyboard Layout:** GB

### Active Features
- **Toggle Key:** OFF (always enabled)
- **Notifications:** Disabled
- **Auto-restart:** Enabled
- **App-specific configs:** Configured for Kitty terminal (CTRL+SHIFT+V) but NOT WORKING currently

### Current Matches
1. `:now` → Current time (HH:MM format)
2. `:triage` → Full triage instructions for task management

### Sops Integration
- ✅ Configured in `espanso.nix` (lines 60-63)
- ✅ Secrets file linked properly
- ✅ Systemd service depends on sops-nix.service

## Organizing Matches with Packages

Espanso supports organizing matches into separate files (packages) in the match directory.
Home Manager creates these as separate `matches.*` sections in `espanso.nix`.

### Current Organization
- `matches.base` → `~/.config/espanso/match/base.yml` (time/date, triage)
- Sops secrets → `~/.config/espanso/match/secrets.yml` (sensitive data)

### Suggested Match Packages to Consider

To add a new package, create a new `matches.*` section in `espanso.nix`:
```nix
matches.package-name = {
  matches = [
    # ... your matches here
  ];
};
```

**AI Prompts** (`matches.ai-prompts`)
- System prompts for different AI tools
- Code review templates
- Documentation generation prompts
- Debugging assistance prompts

**Common Shortcuts** (`matches.shortcuts`)
- Frequently used phrases
- Code snippets
- Symbols and Unicode characters
- Git commit message prefixes

**Development** (`matches.dev`)
- Git commit templates
- Code boilerplate snippets
- Debug print statements
- Common shell commands

**Communication** (`matches.comm`)
- Email templates
- Meeting responses
- Status update templates

## Security Considerations

1. **Secrets in Sops:** API keys, passwords, personal info
2. **Plain Text Allowed:** Public templates, code snippets, generic text
3. **Review Before Commit:** Always check for accidentally exposed secrets
4. **Sops Encryption Key:** Ensure your age key is backed up

## References

- [Official Docs](https://espanso.org/docs/)
- [Match Syntax](https://espanso.org/docs/matches/)
- [Variables](https://espanso.org/docs/matches/extensions/)
- [Forms](https://espanso.org/docs/matches/forms/)
- [Packages](https://espanso.org/docs/packages/)
