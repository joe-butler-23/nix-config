# Declarative Web Apps with Brave on NixOS

This module provides a declarative approach to managing web applications using Brave's app mode on NixOS.

## Overview

Web apps are created as desktop entries that launch Brave in app mode (`--app=`), providing a native-like experience for web applications.

## Declarative vs Imperative

### ❌ Imperative Approach (Not Recommended)
```bash
# Manual file creation outside Nix control
mkdir -p ~/.local/share/applications
cat > ~/.local/share/applications/chatgpt.desktop << EOF
[Desktop Entry]
Exec=brave --app=https://chat.openai.com
EOF
update-desktop-database ~/.local/share/applications
```

**Problems:**
- Files created in mutable locations
- Not tracked by Nix
- Manual steps required
- Won't survive system rebuild

### ✅ Declarative Approach (Recommended)
```nix
# modules/home/web-apps/default.nix
{ pkgs, ... }: {
  xdg.desktopEntries = {
    chatgpt = {
      name = "ChatGPT";
      comment = "ChatGPT AI Assistant Web App";
      icon = "chatgpt";
      exec = "${pkgs.brave}/bin/brave --app=https://chat.openai.com";
      categories = [ "Network" "Office" "Development" ];
      terminal = false;
      keywords = [ "AI" "Chat" "Assistant" "GPT" "OpenAI" ];
    };
  };
}
```

**Benefits:**
- Managed by Nix store
- Reproducible and portable
- Atomic updates with rollback
- Version controlled
- Automatic during system rebuild

## Available Web Apps

This module includes the following web applications:

| App | URL | Category | Description |
|-----|-----|----------|-------------|
| **ChatGPT** | `https://chat.openai.com` | Development | AI Assistant |
| **Gmail** | `https://gmail.com` | Email | Web Email Client |
| **Spotify Web** | `https://open.spotify.com` | Audio | Music Streaming |
| **Twitter** | `https://twitter.com` | Social Media | Social Network |
| **Notion** | `https://notion.so` | Office | Productivity Suite |

## Adding New Web Apps

To add a new web app, edit `modules/home/web-apps/default.nix`:

```nix
xdg.desktopEntries = {
  # Existing apps...
  
  your-app = {
    name = "Your App";
    comment = "Your App Description";
    icon = "your-icon";
    exec = "${pkgs.brave}/bin/brave --app=https://your-app.com";
    categories = [ "Network" "Office" ];
    terminal = false;
    keywords = [ "keyword1" "keyword2" ];
  };
};
```

### Required Fields
- `name`: Display name of the application
- `exec`: Command to launch (use `${pkgs.brave}/bin/brave --app=URL`)

### Optional Fields
- `comment`: Short description
- `icon`: Icon name (must exist in icon theme)
- `categories`: Freedesktop categories
- `terminal`: Whether app runs in terminal (usually `false`)
- `keywords`: Search keywords

## Installation

1. **Import the module** in your `home.nix`:
```nix
{
  imports = [
    ./modules/home/web-apps
    # ... other imports
  ];
}
```

2. **Ensure Brave is available** (should be in your packages):
```nix
{
  home.packages = with pkgs; [
    brave
    # ... other packages
  ];
}
```

3. **Rebuild your configuration**:
```bash
home-manager switch --flake .#joebutler
```

## Usage

After installation, web apps will appear in:
- Application launchers (Rofi, Wofi, etc.)
- Desktop application menus
- Can be launched from terminal using desktop file name

Example:
```bash
# Launch ChatGPT web app
gtk-launch chatgpt
```

## Verification

Check that desktop entries are properly created:
```bash
# List desktop entries
ls -la ~/.local/share/applications/

# Should show symlinks to Nix store
# chatgpt.desktop -> /nix/store/.../chatgpt.desktop

# Test desktop entry
gtk-launch chatgpt
```

## Icon Management

Icons should be available in your icon theme. For custom icons:
1. Place icons in `~/.local/share/icons/`
2. Use standard icon names in desktop entries
3. Consider using icon themes that include web app icons

## Troubleshooting

### Web App Not Appearing
1. Verify home-manager rebuild completed successfully
2. Check desktop entry exists: `ls ~/.local/share/applications/`
3. Update desktop database: `update-desktop-database ~/.local/share/applications`
4. Restart your application launcher

### Brave Not Found
1. Ensure Brave is in your `home.packages`
2. Check Brave installation: `which brave`
3. Verify path in desktop entry matches Nix store

### Icon Missing
1. Check icon theme includes the icon
2. Verify icon name matches desktop entry
3. Consider using generic icon names as fallback

## Advanced Configuration

### Custom Brave Flags
Add additional Brave flags to the `exec` field:
```nix
exec = "${pkgs.brave}/bin/brave --app=https://app.com --disable-web-security --user-data-dir=/tmp/app-profile";
```

### Systemd Integration
For better process management, consider using `app2unit`:
```bash
# Launch web app as systemd service
app2unit chatgpt.desktop
```

### Multiple Profiles
Create separate profiles for different use cases:
```nix
work-apps = {
  xdg.desktopEntries = {
    # Work-related web apps
  };
};

personal-apps = {
  xdg.desktopEntries = {
    # Personal web apps
  };
};
```

## Migration from Imperative Setup

If you have existing imperative web apps:

1. **Backup current desktop entries**:
```bash
cp -r ~/.local/share/applications ~/backup-applications/
```

2. **Remove imperative entries**:
```bash
rm ~/.local/share/applications/your-app.desktop
```

3. **Add to declarative configuration** (see Adding New Web Apps)

4. **Rebuild**:
```bash
home-manager switch --flake .#joebutler
```

5. **Verify**:
```bash
ls -la ~/.local/share/applications/
# Should show symlinks to Nix store
```

## Best Practices

1. **Use specific URLs** (avoid redirects)
2. **Choose appropriate categories** for better organization
3. **Include relevant keywords** for searchability
4. **Test web apps work in Brave app mode** before adding
5. **Keep icon names consistent** with web app branding
6. **Version control your web app configurations**

## Related Tools

- **app2unit**: Launch desktop entries as systemd services
- **gtk-launch**: Test desktop entries from command line
- **desktop-file-validate**: Validate desktop entry syntax
- **update-desktop-database**: Update desktop entry database

## References

- [Brave Browser Documentation](https://support.brave.com/)
- [Freedesktop Desktop Entry Specification](https://specifications.freedesktop.org/desktop-entry-spec/)
- [Home Manager XDG Options](https://nix-community.github.io/home-manager/options.xhtml#opt-xdg.desktopEntries)
- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
