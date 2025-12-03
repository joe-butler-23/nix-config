# Shell & Terminal Environment

Configuration and troubleshooting for ZSH + Powerlevel10k + Kitty setup.

## ZSH Configuration

### Common ZSH Issues

**Slow shell startup:**
```bash
# Profile startup time
time zsh -i -c exit

# Common culprits
# - Too many plugins
# - Heavy prompt themes
# - Unoptimized completions

# Quick fixes
autoload -U compinit && compinit  # Rebuild completions
```

**Plugin conflicts:**
```bash
# Check loaded plugins
echo $plugins

# Disable problematic plugins temporarily
# Edit ~/.zshrc and comment out specific plugins
```

### ZSH Best Practices

```bash
# Use zinit or similar for fast plugin loading
# Lazy load heavy completions
# Cache expensive operations

# Example .zshrc structure:
# 1. Environment variables
# 2. Path modifications
# 3. Plugin management
# 4. Completions
# 5. Aliases and functions
# 6. Prompt setup (P10K)
```

## Powerlevel10k (P10K)

### P10K Quick Fixes

```bash
# Reconfigure P10K completely
p10k configure

# Show configuration wizard again
p10k configure --reset

# Reload P10K without restarting shell
source ~/.p10k.zsh
```

### P10K Troubleshooting

**Prompt not showing correctly:**
```bash
# Check if P10K is loaded
echo $POWERLEVEL9K_MODE

# Verify font installation
# P10K needs Nerd Fonts or MesloLGS fonts
# Check in Kitty if fonts are properly configured
```

**Slow prompt:**
```bash
# Check which segments are slow
# Edit ~/.p10k.zsh and disable expensive segments:
# - vcs (git status)
# - node_version
# - python_version
# etc.
```

### P10K Configuration Tips

```bash
# In ~/.p10k.zsh, useful segments for development:
typeset -g POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(
  os_icon
  dir
  vcs
  newline
  prompt_char
)

typeset -g POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(
  status
  command_execution_time
  background_jobs
  direnv                  # Shows when direnv is active
  nix_shell              # Shows when in nix develop
)
```

## Kitty Terminal

### Kitty Configuration

**Location:** `~/.config/kitty/kitty.conf`

```bash
# Essential settings for development
font_family      MesloLGS NF
font_size        12
cursor_shape     beam
copy_on_select   yes

# Useful for development
enabled_layouts  tall:bias=50;full_size=1
tab_bar_edge     top
tab_bar_style    powerline

# NixOS-friendly scrollback
scrollback_lines 10000
```

### Kitty Shortcuts & Features

```bash
# Window management
Ctrl+Shift+Enter    # New window
Ctrl+Shift+W        # Close window
Ctrl+Shift+]        # Next window
Ctrl+Shift+[        # Previous window

# Tab management
Ctrl+Shift+T        # New tab
Ctrl+Shift+Q        # Close tab
Ctrl+Shift+Right    # Next tab
Ctrl+Shift+Left     # Previous tab

# Layouts
Ctrl+Shift+L        # Next layout
Ctrl+Shift+Alt+T    # Set tab title
```

### Kitty + Development Tools

```bash
# Better SSH experience
kitty +kitten ssh hostname

# File transfer
kitty +kitten transfer file.txt hostname:

# Themes
kitty +kitten themes

# Unicode input
Ctrl+Shift+U        # Unicode input mode
```

## Development Workflow Integration

### Directory-based environments

```bash
# Use direnv for per-project environments
echo "use flake" > .envrc
direnv allow

# ZSH will show direnv status in prompt
# P10K can display current environment
```

### Git integration

```bash
# ZSH git aliases (if using Oh My Zsh git plugin)
alias gst='git status'
alias gco='git checkout'
alias gcm='git commit -m'
alias gp='git push'
alias gl='git pull'

# P10K shows git status in prompt automatically
```

### Path management

```bash
# In ~/.zshrc, manage PATH for development
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

# For NixOS, prefer nix develop over modifying global PATH
# Use direnv + flake.nix for project-specific tools
```

## Troubleshooting Commands

### Shell debugging
```bash
# Check shell configuration issues
zsh -x    # Run with tracing

# Check environment
env | sort    # Show all environment variables
echo $SHELL   # Verify shell
echo $TERM    # Check terminal type
```

### Terminal debugging
```bash
# Test color support
curl -s https://gist.githubusercontent.com/HaleTom/89ffe32783f89f403bba96bd7bcd1263/raw/ | bash

# Check font rendering
echo "Icons: "
echo "Git:  "

# Test kitty features
kitty +kitten show_key -m kitty
```

### Performance profiling
```bash
# Profile ZSH startup
time ( zsh -i -c exit )

# Profile P10K segments
# Set POWERLEVEL9K_DEBUG=1 in ~/.zshrc temporarily
```

## Common Issues & Solutions

**Issue:** Terminal looks broken after SSH
```bash
# Reset terminal
reset
# Or
tput reset
# Or
echo -e "\033c"
```

**Issue:** Colors wrong in terminal applications
```bash
# Check TERM variable
echo $TERM    # Should be xterm-kitty or similar

# Set in ~/.zshrc if needed
export TERM=xterm-256color
```

**Issue:** Clipboard not working
```bash
# Kitty clipboard integration
echo "test" | kitty +kitten clipboard
kitty +kitten clipboard --get-clipboard
```

**Issue:** Font rendering problems
```bash
# Install proper fonts for P10K
nix-env -iA nixpkgs.meslo-lgs-nf

# Configure in kitty.conf
font_family MesloLGS NF
```
