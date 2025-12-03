# Hyprland Desktop Environment

Hyprland window manager configuration, keybindings, and troubleshooting.

## Essential Hyprland Concepts

### Window Management Philosophy
- **Tiling by default** with floating override
- **Workspaces** for organization
- **Monitors** with independent workspaces
- **Master-stack** or **dwindle** layouts

### Configuration Location
- **Main config:** `~/.config/hypr/hyprland.conf`
- **Autostart:** Usually in `exec-once` sections
- **Per-app rules:** Window rules for specific applications

## Default Keybindings

### Core Navigation
```bash
# Window focus
Super + H/J/K/L         # Focus left/down/up/right
Super + Arrow Keys      # Focus direction

# Window movement
Super + Shift + H/J/K/L # Move window
Super + Shift + Arrows  # Move window direction

# Workspaces
Super + 1-9             # Switch to workspace N
Super + Shift + 1-9     # Move window to workspace N
Super + ScrollWheel     # Cycle workspaces
```

### Essential Actions
```bash
# Applications
Super + Enter           # Terminal (usually kitty)
Super + D               # Application launcher (wofi/rofi)
Super + Shift + Q       # Close window

# Layouts
Super + Space           # Toggle floating
Super + F               # Toggle fullscreen
Super + P               # Toggle pseudo (master-stack)
```

### System Controls
```bash
# Session management
Super + Shift + E       # Exit Hyprland
Super + L               # Lock screen
Super + Shift + R       # Reload configuration

# Media & volume (if configured)
XF86AudioRaiseVolume    # Volume up
XF86AudioLowerVolume    # Volume down
XF86AudioMute           # Mute toggle
```

## Configuration Patterns

### Window Rules
```bash
# In hyprland.conf
windowrule = float, ^(pavucontrol)$
windowrule = size 800 600, ^(pavucontrol)$
windowrule = move 100 100, ^(pavucontrol)$

# For development
windowrule = workspace 2, ^(code-oss)$
windowrule = workspace 3, ^(firefox)$
windowrule = float, ^(kitty-floating)$
```

### Workspace Setup
```bash
# Bind specific apps to workspaces
bind = SUPER, 1, workspace, 1      # Terminal/general
bind = SUPER, 2, workspace, 2      # Code/development
bind = SUPER, 3, workspace, 3      # Browser
bind = SUPER, 4, workspace, 4      # Communication
bind = SUPER, 5, workspace, 5      # Media

# Move windows to workspaces
bind = SUPER_SHIFT, 1, movetoworkspace, 1
bind = SUPER_SHIFT, 2, movetoworkspace, 2
```

### Multi-Monitor Configuration
```bash
# Monitor setup (adjust for your displays)
monitor = DP-1, 2560x1440@144, 0x0, 1
monitor = HDMI-1, 1920x1080@60, 2560x0, 1

# Workspace binding to monitors
workspace = 1, monitor:DP-1
workspace = 2, monitor:DP-1
workspace = 3, monitor:HDMI-1
workspace = 4, monitor:HDMI-1
```

## Development-Focused Setup

### IDE/Editor Optimization
```bash
# For VS Code/editors
windowrule = workspace 2, ^(code)$
windowrule = workspace 2, ^(code-oss)$
windowrule = nofocus, ^(code)$ # Prevent stealing focus

# Terminal configurations
windowrule = float, title:^(floating_terminal)$
windowrule = size 1200 800, title:^(floating_terminal)$
windowrule = move cursor -50% -50%, title:^(floating_terminal)$
```

### Useful Development Keybinds
```bash
# Quick terminal access
bind = SUPER, grave, exec, kitty --title floating_terminal
bind = SUPER, T, exec, kitty

# Screenshot for documentation
bind = SUPER, S, exec, grimblast copy area
bind = SUPER_SHIFT, S, exec, grimblast save area

# Application-specific
bind = SUPER, C, exec, code
bind = SUPER, B, exec, firefox
```

### Layout for Coding
```bash
# Master-stack layout for coding
general {
    layout = master
}

master {
    new_is_master = false
    new_on_top = true
    orientation = left
    inherit_fullscreen = true
}
```

## Troubleshooting

### Common Issues

**Hyprland won't start:**
```bash
# Check logs
journalctl -u display-manager
cat ~/.local/share/hyprland/hyprland.log

# Test configuration
hyprland --config ~/.config/hypr/hyprland.conf
```

**Applications not launching:**
```bash
# Check if running from terminal
kitty &
code &

# Verify executable paths
which kitty
which code
echo $PATH
```

**Window rules not working:**
```bash
# Get window class names
hyprctl clients | grep class

# Test window rules
hyprctl keyword windowrule "float,^(pavucontrol)$"
```

### Performance Issues

**High CPU usage:**
```bash
# Check compositing settings
decoration {
    blur = no          # Disable if causing issues
    drop_shadow = no   # Disable expensive effects
}

animations {
    enabled = no       # Disable for performance
}
```

**Screen tearing:**
```bash
# In hyprland.conf
misc {
    vrr = 1            # Variable refresh rate
    vfr = true         # Variable framerate
}
```

### Monitor/Display Issues

**External monitor not detected:**
```bash
# List available outputs
hyprctl monitors

# Force monitor configuration
monitor = HDMI-1, 1920x1080@60, auto, 1

# Reload configuration
hyprctl reload
```

**Resolution problems:**
```bash
# Set custom resolution
monitor = DP-1, 2560x1440@144, 0x0, 1

# Or use preferred resolution
monitor = DP-1, preferred, auto, 1
```

## Essential Tools Integration

### Application Launcher
```bash
# Wofi (Wayland-native)
bind = SUPER, D, exec, wofi --show drun

# Rofi (X11, works with Wayland)
bind = SUPER, D, exec, rofi -show drun -show-icons
```

### Screenshots & Screen Recording
```bash
# Grimblast (Hyprland-optimized)
bind = SUPER, S, exec, grimblast copy area
bind = SUPER_SHIFT, S, exec, grimblast save area ~/Pictures/
bind = SUPER_CTRL, S, exec, grimblast copy screen

# For screen recording
bind = SUPER, R, exec, wf-recorder -g "$(slurp)"
```

### Status Bar Integration
```bash
# Waybar configuration
exec-once = waybar

# Eww (alternative)
exec-once = eww daemon && eww open bar

# Ags (advanced)
exec-once = ags
```

## Configuration Debugging

### Check current configuration
```bash
# View active configuration
hyprctl getoption general:layout
hyprctl getoption decoration:blur

# List all windows
hyprctl clients

# List all workspaces
hyprctl workspaces

# Monitor information
hyprctl monitors
```

### Live configuration changes
```bash
# Test keybinds without restart
hyprctl keyword bind "SUPER, T, exec, kitty"

# Test window rules
hyprctl keyword windowrule "float,^(test)$"

# Change layout temporarily
hyprctl keyword general:layout "dwindle"
```

### Environment Variables
```bash
# For development tools in Hyprland
env = EDITOR, code
env = BROWSER, firefox
env = TERMINAL, kitty

# Wayland-specific
env = XDG_CURRENT_DESKTOP, Hyprland
env = XDG_SESSION_TYPE, wayland
env = XDG_SESSION_DESKTOP, Hyprland
```
