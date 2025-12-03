# NixOS Development Gotchas

Comprehensive NixOS-specific development patterns and troubleshooting.

## Core Concepts

### Clean Git Tree Before NixOS Rebuilds

**Problem:** `nixos-rebuild` won't see uncommitted files, causing mysterious build failures
```bash
nixos-rebuild switch  # ❌ May fail silently if new files not in git
```

**Solution:** Always commit or add files first
```bash
git status                    # Check what's uncommitted
git add .                    # Add new files
git commit -m "Ready for rebuild"
nixos-rebuild switch         # ✅ Now sees all files
```

**Why:** Nix builds are based on git trees, not filesystem state. New files that aren't tracked by git simply don't exist to Nix.

### Use `#!/usr/bin/env` for Shebangs

**Problem:** Traditional paths don't exist on NixOS
```bash
#!/bin/bash        # ❌ Fails on NixOS
#!/usr/bin/python3 # ❌ Fails on NixOS
```

**Solution:** Always use env
```bash
#!/usr/bin/env bash
#!/usr/bin/env python3
#!/usr/bin/env node
```

### Use Flake Dev Shells Instead of Global Installs

**Problem:** Global package managers break isolation
```bash
npm install -g typescript  # ❌ Don't do this
pip install --user black   # ❌ Don't do this
```

**Solution:** Declare in flake.nix
```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { nixpkgs, ... }: {
    devShells.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.mkShell {
      buildInputs = with nixpkgs.legacyPackages.x86_64-linux; [
        nodePackages.typescript
        python3Packages.black
      ];
    };
  };
}
```

Then: `nix develop`

### Prefer Available Tools Over Missing Ones

**Problem:** Some packages aren't available in nixpkgs
```bash
npx tsx script.ts  # ❌ tsx not in nixpkgs
```

**Solution:** Use alternatives that are available
```bash
bun run script.ts           # ✅ bun available
ts-node script.ts           # ✅ ts-node available
nix develop -c npx tsx      # ✅ or get from npm in shell
```

### Use Relative Paths, Not FHS Assumptions

**Problem:** Standard paths don't exist
```bash
/usr/local/bin/tool  # ❌ Doesn't exist
/opt/app/bin/tool    # ❌ Doesn't exist
```

**Solution:** Use relative paths or environment variables
```bash
./tools/script.sh          # ✅ Relative path
$HOME/.local/bin/tool      # ✅ Environment variable
$(dirname "$0")/helper.sh  # ✅ Script-relative path
```

## Package Equivalents

| Traditional | NixOS flake.nix |
|-------------|----------------|
| `apt install build-essential` | `stdenv.cc gnumake` |
| `apt install curl` | `curl` |
| `npm install -g X` | `nodePackages.X` |
| `pip install X` | `python3Packages.X` |
| `gem install X` | `rubyPackages.X` |
| `cargo install X` | Often `X` directly or check nixpkgs |

## Debugging Commands

### Command Not Found
```bash
# Debug what's available
which python3           # Should show /nix/store/... path
echo $PATH              # Check if binary in PATH
nix search nixpkgs python3  # Find package name
nix-locate bin/python3  # Find which package provides binary
```

### Package Search Strategies
```bash
# Search for packages
nix search nixpkgs typescript
nix search nixpkgs python | grep -i flask

# Check package contents
nix show-derivation nixpkgs#python3
```

### Environment Variables in Flake
```nix
devShells.default = pkgs.mkShell {
  buildInputs = [ pkgs.python3 ];
  shellHook = ''
    export API_KEY="dev-key"
    export PYTHONPATH="$PWD/src:$PYTHONPATH"
    if [ -f .env ]; then source .env; fi
    echo "Development environment ready!"
  '';
};
```

## Migration Strategies

### From apt/yum systems
1. Identify required packages with `dpkg -l` or `rpm -qa`
2. Search nixpkgs equivalents
3. Create flake.nix with dev shell
4. Test with `nix develop`

### From Homebrew (macOS)
1. List with `brew list`
2. Most packages have direct nixpkgs equivalents
3. Language-specific packages usually in `nodePackages`, `python3Packages`, etc.

### From language-specific managers
- **npm global packages** → `nodePackages.X` in flake
- **pip user packages** → `python3Packages.X` in flake
- **gem user installs** → `rubyPackages.X` in flake
- **cargo installs** → Often available as `X` in nixpkgs

## Advanced Patterns

### Overlay for Custom Packages
```nix
# In flake.nix
outputs = { nixpkgs, ... }: let
  overlay = final: prev: {
    myCustomTool = prev.writeShellScriptBin "my-tool" ''
      echo "Custom script here"
    '';
  };
in {
  devShells.x86_64-linux.default =
    (nixpkgs.legacyPackages.x86_64-linux.extend overlay).mkShell {
      buildInputs = [ myCustomTool ];
    };
};
```

### Multiple Dev Shells
```nix
devShells.x86_64-linux = {
  default = pkgs.mkShell { /* basic setup */ };
  python = pkgs.mkShell {
    buildInputs = [ pkgs.python3 pkgs.python3Packages.pip ];
  };
  node = pkgs.mkShell {
    buildInputs = [ pkgs.nodejs pkgs.nodePackages.npm ];
  };
};
```

Access with: `nix develop .#python` or `nix develop .#node`
