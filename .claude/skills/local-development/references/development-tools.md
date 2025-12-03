# Development Tools & Configuration

Development toolchain setup, editor configuration, and workflow optimization.

## Editor Setup

### VS Code / VS Codium
**Location:** `~/.config/Code/User/settings.json`

```json
{
  "terminal.integrated.shell.linux": "/run/current-system/sw/bin/zsh",
  "terminal.integrated.fontFamily": "MesloLGS NF",

  "nix.enableLanguageServer": true,
  "nix.serverPath": "nil",

  "git.enableSmartCommit": true,
  "git.autofetch": true,

  "files.watcherExclude": {
    "**/target/**": true,
    "**/.direnv/**": true,
    "**/node_modules/**": true
  }
}
```

**Essential Extensions for NixOS:**
- Nix IDE
- direnv
- GitLens
- Remote Development (for containers)

### Neovim (if used)
**Location:** `~/.config/nvim/`

```lua
-- Essential for NixOS development
require('lspconfig').nil_ls.setup{}  -- Nix LSP
require('lspconfig').rnix.setup{}    -- Alternative Nix LSP

-- Direnv integration
vim.cmd([[
  augroup direnv
    autocmd!
    autocmd VimEnter * :silent !direnv reload
  augroup END
]])
```

## Git Configuration

### Global Git Config
```bash
# Essential Git settings
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"

# Better defaults
git config --global init.defaultBranch main
git config --global pull.rebase false
git config --global push.default simple

# NixOS-friendly editor
git config --global core.editor "code --wait"
# Or for terminal
git config --global core.editor "nvim"
```

### Git Aliases
```bash
# Useful aliases
git config --global alias.st status
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
git config --global alias.unstage 'reset HEAD --'
git config --global alias.last 'log -1 HEAD'
git config --global alias.visual '!gitk'

# Advanced aliases
git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
```

### Git Hooks Integration
```bash
# Pre-commit hooks with Nix
# In flake.nix:
{
  devShells.default = pkgs.mkShell {
    buildInputs = [ pkgs.pre-commit ];
    shellHook = ''
      pre-commit install
    '';
  };
}
```

## Language-Specific Toolchains

### Python Development
```nix
# In flake.nix
devShells.python = pkgs.mkShell {
  buildInputs = with pkgs; [
    python3
    python3Packages.pip
    python3Packages.virtualenv
    python3Packages.black
    python3Packages.flake8
    python3Packages.mypy
    python3Packages.pytest
  ];

  shellHook = ''
    # Create virtual environment if not exists
    if [ ! -d ".venv" ]; then
      python -m venv .venv
    fi
    source .venv/bin/activate

    export PYTHONPATH="$PWD/src:$PYTHONPATH"
  '';
};
```

### Node.js Development
```nix
# In flake.nix
devShells.node = pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs
    nodePackages.npm
    nodePackages.typescript
    nodePackages.eslint
    nodePackages.prettier
    nodePackages."@types/node"
  ];

  shellHook = ''
    export NODE_ENV=development
    # Auto npm install if package.json changed
    if [ package.json -nt node_modules/.package-lock.json ]; then
      npm install
    fi
  '';
};
```

### Rust Development
```nix
devShells.rust = pkgs.mkShell {
  buildInputs = with pkgs; [
    rustc
    cargo
    rustfmt
    rust-analyzer
    clippy
  ];

  RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
};
```

### Go Development
```nix
devShells.go = pkgs.mkShell {
  buildInputs = with pkgs; [
    go
    gopls
    gotools
    go-tools
  ];

  shellHook = ''
    export GOPATH="$HOME/go"
    export PATH="$GOPATH/bin:$PATH"
  '';
};
```

## Container & Virtualization

### Docker on NixOS
```nix
# In system configuration
virtualisation.docker.enable = true;
users.users.joebutler.extraGroups = [ "docker" ];

# Development with Docker Compose
devShells.docker = pkgs.mkShell {
  buildInputs = with pkgs; [
    docker
    docker-compose
    dive        # Docker image explorer
    hadolint    # Dockerfile linter
  ];
};
```

### Podman (Docker alternative)
```nix
# System config
virtualisation.podman.enable = true;
virtualisation.podman.dockerCompat = true;

# Development shell
devShells.podman = pkgs.mkShell {
  buildInputs = with pkgs; [
    podman
    podman-compose
    buildah
    skopeo
  ];
};
```

## Database Tools

### PostgreSQL Development
```nix
devShells.postgres = pkgs.mkShell {
  buildInputs = with pkgs; [
    postgresql
    pgcli           # Better psql client
    pg_dump
    pg_restore
  ];

  shellHook = ''
    export PGDATA="$PWD/.postgres"
    if [ ! -d "$PGDATA" ]; then
      initdb --auth-local=peer --auth-host=md5
    fi
  '';
};
```

### Redis Development
```nix
devShells.redis = pkgs.mkShell {
  buildInputs = with pkgs; [
    redis
    redis-cli
  ];

  shellHook = ''
    # Start Redis in background if not running
    if ! pgrep redis-server > /dev/null; then
      redis-server --daemonize yes --port 6379
    fi
  '';
};
```

## Build Tools & CI

### Make & Build Systems
```nix
devShells.build = pkgs.mkShell {
  buildInputs = with pkgs; [
    gnumake
    cmake
    ninja
    meson
    pkg-config

    # For different languages
    gcc
    clang
    rustc
    go
  ];
};
```

### CI/CD Tools
```nix
devShells.ci = pkgs.mkShell {
  buildInputs = with pkgs; [
    gh              # GitHub CLI
    act             # Local GitHub Actions
    gitlab-runner   # GitLab CI
    drone-cli       # Drone CI
  ];
};
```

## Monitoring & Debugging

### System Monitoring
```nix
devShells.monitoring = pkgs.mkShell {
  buildInputs = with pkgs; [
    htop
    btop            # Better htop
    iotop
    nethogs         # Network usage per process
    strace
    ltrace
    lsof

    # Network tools
    nmap
    wireshark-cli
    tcpdump

    # Performance
    perf-tools
    flamegraph
  ];
};
```

### Application Debugging
```nix
devShells.debug = pkgs.mkShell {
  buildInputs = with pkgs; [
    gdb
    valgrind
    rr              # Deterministic debugger

    # Language-specific
    nodejs          # Node.js debugging
    python3Packages.pudb  # Python debugger
  ];
};
```

## Common Workflow Commands

### Project Initialization
```bash
# Initialize new project with flake
nix flake init
# Or with template
nix flake init --template github:NixOS/templates#python

# Initialize git
git init
git add flake.nix
git commit -m "Initial commit"

# Enter development environment
nix develop
```

### Daily Development
```bash
# Enter project environment
cd project && nix develop

# Or with direnv
echo "use flake" > .envrc && direnv allow

# Update dependencies
nix flake update

# Build project
nix build

# Run tests
nix develop -c pytest
# Or
nix develop -c npm test
```

### Environment Management
```bash
# List available shells
nix flake show

# Enter specific environment
nix develop .#python
nix develop .#node

# Run command in environment
nix develop -c python script.py
nix develop .#node -c npm start
```

## Troubleshooting

### Common Issues

**Editor can't find language server:**
```bash
# Check if LSP is available in current environment
which rust-analyzer
which typescript-language-server

# Add to flake.nix if missing
buildInputs = [ pkgs.rust-analyzer ];
```

**Git operations slow:**
```bash
# Check if in large repository
git status --porcelain | wc -l

# Use gitignore properly
echo ".direnv/" >> .gitignore
echo "result" >> .gitignore
```

**Dependencies not found:**
```bash
# Check current environment
echo $NIX_PATH
nix develop --print-dev-env

# Rebuild environment
nix develop --rebuild
```

### Performance Optimization

**Faster rebuilds:**
```bash
# Use binary cache
nix-env --option extra-binary-caches https://cache.nixos.org/

# Parallel builds
nix build --max-jobs 8
```

**Reduce disk usage:**
```bash
# Clean old generations
nix-collect-garbage -d

# Clean build artifacts
nix store gc
```
