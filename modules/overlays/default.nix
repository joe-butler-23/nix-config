# ========================================
# CUSTOM OVERLAYS
# ========================================
# This file acts as a "patch layer" on top of standard Nixpkgs.
# It injects custom tools that are not available in the official repositories
# or where we need a specific version/fork.
#
# TOOLS PROVIDED:
# 1. opencode: The OpenCode CLI agent (https://github.com/sst/opencode)
# 2. gemini: The Google Gemini CLI (https://github.com/google-gemini/gemini-cli)
# 3. claude: The Anthropic Claude Code CLI (https://github.com/anthropic/claude-code)
# 4. codex: The OpenAI Codex CLI (https://github.com/openai/codex)
# 5. openmemory-js: The OpenMemory JavaScript SDK (https://github.com/CaviraOSS/OpenMemory)
# 5. byterover: The ByteRover Cipher AI agent framework (https://byterover.dev)
# 6. openmemory-js: The OpenMemory JavaScript SDK (https://github.com/CaviraOSS/OpenMemory)
# 7. zed-editor: The Zed Editor (https://github.com/zed-industries/zed)
#
# USAGE:
# These packages become available in `pkgs` automatically (e.g., pkgs.opencode).
#
# AUTOMATIC UPDATES:
# A systemd service (overlay-updates) runs daily to check for new versions
# of these packages and automatically updates this file. See:
# - modules/services/overlay-updates.nix - Service configuration
# - Systemd timer: overlay-updates.timer - Runs at 00:00 daily
#
# TECHNICAL DETAILS:
# - _final: The "future" package set (unused here)
# - prev: The "current" package set (used to build our new tools)
# ========================================
# This file acts as a "patch layer" on top of the standard Nixpkgs.
# It injects custom tools that are not available in the official repositories
# or where we need a specific version/fork.
#
# TOOLS PROVIDED:
# 1. opencode: The OpenCode CLI agent (https://github.com/sst/opencode)
# 2. gemini: The Google Gemini CLI (https://github.com/google-gemini/gemini-cli)
# 3. claude: The Anthropic Claude Code CLI (https://github.com/anthropics/claude-code)
# 4. codex: The OpenAI Codex CLI (https://github.com/openai/codex)
#
# USAGE:
# These packages become available in `pkgs` automatically (e.g., pkgs.opencode).
#
# TECHNICAL DETAILS:
# - _final: The "future" package set (unused here)
# - prev: The "current" package set (used to build our new tools)
# ========================================
_: _final: prev: {
  opencode = prev.stdenv.mkDerivation rec {
    pname = "opencode";
    version = "1.1.3";

    src = prev.fetchurl {
      url = "https://github.com/sst/opencode/releases/download/v${version}/opencode-linux-x64.tar.gz";
      sha256 = "10lbglamc5wivbyhajn402bm1qmz8w95kgn6jyyazbdrn1fchjp7";
    };

    nativeBuildInputs = [prev.autoPatchelfHook];

    # Runtime dependencies
    buildInputs = [
      prev.stdenv.cc.cc.lib
      prev.zlib
    ];

    sourceRoot = ".";
    dontStrip = true;

    installPhase = ''
      runHook preInstall
      install -Dm755 opencode $out/bin/opencode
      runHook postInstall
    '';

    # Update Script: Fetches the latest version tag from GitHub and updates this file
    passthru.updateScript = prev.writeShellScript "update-opencode" ''
      set -euo pipefail

      # Get latest version
      LATEST=$(curl -sL https://api.github.com/repos/sst/opencode/releases/latest | jq -r '.tag_name' | sed 's/^v//')

      echo "Latest version: $LATEST"

      # Calculate new hash first
      URL="https://github.com/sst/opencode/releases/download/v$LATEST/opencode-linux-x64.tar.gz"
      HASH=$(nix-prefetch-url "$URL")

      echo "Calculated hash: $HASH"

      # Update version in file
      sed -i '/pname = "opencode"/,/^  };/ s|^    version = ".*"|    version = "'$LATEST'"|' modules/overlays/default.nix

      # Update hash in file
      sed -i '/pname = "opencode"/,/^  };/ s|^      sha256 = ".*"|      sha256 = "'$HASH'"|' modules/overlays/default.nix

      echo "Updated opencode to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "OpenCode CLI";
      homepage = "https://github.com/sst/opencode";
      license = licenses.mit;
      platforms = platforms.linux;
    };
  };

  gemini = prev.stdenv.mkDerivation rec {
    pname = "gemini-cli";
    version = "0.24.0-nightly.20260103.30f5c4af4";

    src = prev.fetchurl {
      url = "https://github.com/google-gemini/gemini-cli/releases/download/v${version}/gemini.js";
      sha256 = "0wsbzk6yzz5pyh37767fxd5d3p8dnzlqpvcdfr6yr2pnv5hidgh9";
    };

    dontUnpack = true;

    nativeBuildInputs = [prev.makeWrapper];

    installPhase = ''
      mkdir -p $out/libexec $out/bin
      cp $src $out/libexec/gemini.js
      makeWrapper ${prev.nodejs}/bin/node $out/bin/gemini \
        --add-flags "$out/libexec/gemini.js"
    '';

    passthru.updateScript = prev.writeShellScript "update-gemini-cli" ''
      set -euo pipefail

      # Get latest version
      LATEST=$(curl -sL "https://api.github.com/repos/google-gemini/gemini-cli/tags" | jq -r '.[0].name' | sed 's/^v//')

      echo "Latest version: $LATEST"

      # Calculate new hash first
      URL="https://github.com/google-gemini/gemini-cli/releases/download/v$LATEST/gemini.js"
      HASH=$(nix-prefetch-url "$URL")

      echo "Calculated hash: $HASH"

      # Update version in file
      sed -i '/pname = "gemini-cli"/,/^  };/ s|^    version = ".*"|    version = "'$LATEST'"|' modules/overlays/default.nix

      # Update hash in file
      sed -i '/pname = "gemini-cli"/,/^  };/ s|^      sha256 = ".*"|      sha256 = "'$HASH'"|' modules/overlays/default.nix

      echo "Updated gemini-cli to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "Gemini CLI";
      homepage = "https://github.com/google-gemini/gemini-cli";
      license = licenses.asl20;
      platforms = platforms.all;
    };
  };

  claude = prev.stdenv.mkDerivation rec {
    pname = "claude-code";
    version = "2.0.76";

    src = prev.fetchurl {
      url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
      sha256 = "1ndrj51yfgkp1xgph5r3v6n946rlamj3v0y6wk4114wfzwv9k8zw";
    };

    nativeBuildInputs = [prev.makeWrapper];

    installPhase = ''
      mkdir -p $out/libexec $out/bin
      cp -r * $out/libexec/
      makeWrapper ${prev.nodejs}/bin/node $out/bin/claude \
        --add-flags "$out/libexec/cli.js"
    '';

    passthru.updateScript = prev.writeShellScript "update-claude-code" ''
      set -euo pipefail

      # Get latest version
      LATEST=$(curl -sL https://registry.npmjs.org/@anthropic-ai/claude-code/latest | jq -r .version)

      echo "Latest version: $LATEST"

      # Calculate new hash first
      URL="https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-$LATEST.tgz"
      HASH=$(nix-prefetch-url "$URL")

      echo "Calculated hash: $HASH"

      # Update version in file
      sed -i '/pname = "claude-code"/,/^  };/ s|^    version = ".*"|    version = "'$LATEST'"|' modules/overlays/default.nix

      # Update hash in file
      sed -i '/pname = "claude-code"/,/^  };/ s|^      sha256 = ".*"|      sha256 = "'$HASH'"|' modules/overlays/default.nix

      echo "Updated claude-code to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "Claude Code CLI";
      homepage = "https://github.com/anthropics/claude-code";
      license = licenses.isc; # Based on npm output if available, or generic
      platforms = platforms.all;
    };
  };

  codex = prev.stdenv.mkDerivation rec {
    pname = "codex";
    version = "0.77.0";

    src = prev.fetchurl {
      url = "https://github.com/openai/codex/releases/download/rust-v${version}/codex-x86_64-unknown-linux-gnu.tar.gz";
      sha256 = "12rq4znrhdpjs7pm4prczllljc8m09j80py68j7149cnrnydzlal";
    };

    nativeBuildInputs = [prev.autoPatchelfHook];

    buildInputs = [
      prev.stdenv.cc.cc.lib
      prev.zlib
      prev.openssl
    ];

    sourceRoot = ".";

    installPhase = ''
      runHook preInstall
      install -Dm755 codex-x86_64-unknown-linux-gnu $out/bin/codex
      runHook postInstall
    '';

    passthru.updateScript = prev.writeShellScript "update-codex" ''
      set -euo pipefail

      # Get latest version
      LATEST=$(curl -sL https://api.github.com/repos/openai/codex/releases/latest | jq -r '.tag_name' | sed 's/^rust-v//')

      echo "Latest version: $LATEST"

      # Calculate new hash first
      URL="https://github.com/openai/codex/releases/download/rust-v$LATEST/codex-x86_64-unknown-linux-gnu.tar.gz"
      HASH=$(nix-prefetch-url "$URL")

      echo "Calculated hash: $HASH"

      # Update version in file
      sed -i '/pname = "codex"/,/^  };/ s|^    version = ".*"|    version = "'$LATEST'"|' modules/overlays/default.nix

      # Update hash in file
      sed -i '/pname = "codex"/,/^  };/ s|^      sha256 = ".*"|      sha256 = "'$HASH'"|' modules/overlays/default.nix

      echo "Updated codex to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "Codex CLI";
      homepage = "https://github.com/openai/codex";
      license = licenses.asl20;
      platforms = platforms.linux;
    };
  };

  byterover = prev.stdenv.mkDerivation rec {
    pname = "byterover-cipher";
    version = "0.3.0";

    src = prev.fetchurl {
      url = "https://registry.npmjs.org/@byterover/cipher/-/cipher-${version}.tgz";
      sha256 = "14ji50k41v04nfgfa96q6607g21rb6b8kxyy8fz321gvy65l3x92";
    };

    nativeBuildInputs = [prev.makeWrapper];

    installPhase = ''
      mkdir -p $out/libexec $out/bin
      cp -r * $out/libexec/
      makeWrapper ${prev.nodejs}/bin/node $out/bin/byterover \
        --add-flags "$out/libexec/dist/src/app/index.cjs"
    '';

    passthru.updateScript = prev.writeShellScript "update-byterover" ''
      set -euo pipefail

      # Get latest version
      LATEST=$(curl -sL https://registry.npmjs.org/@byterover/cipher/latest | jq -r .version)

      echo "Latest version: $LATEST"

      # Calculate new hash first
      URL="https://registry.npmjs.org/@byterover/cipher/-/cipher-$LATEST.tgz"
      HASH=$(nix-prefetch-url "$URL")

      echo "Calculated hash: $HASH"

      # Update version in file
      sed -i '/pname = "byterover-cipher"/,/^  };/ s|^    version = ".*"|    version = "'$LATEST'"|' modules/overlays/default.nix

      # Update hash in file
      sed -i '/pname = "byterover-cipher"/,/^  };/ s|^      sha256 = ".*"|      sha256 = "'$HASH'"|' modules/overlays/default.nix

      echo "Updated byterover-cipher to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "ByteRover Cipher - Memory-powered AI agent framework";
      homepage = "https://byterover.dev";
      license = licenses.elastic20;
      platforms = platforms.all;
    };
  };

  openmemory-js = prev.stdenv.mkDerivation rec {
    pname = "openmemory-js";
    version = "1.3.2";

    src = prev.fetchurl {
      url = "https://registry.npmjs.org/openmemory-js/-/openmemory-js-${version}.tgz";
      sha256 = "0zm8b85gd1xhwakd07hmn6vn4w2m3az3sy7dhlmmawqg9899j8k0";
    };

    nativeBuildInputs = [prev.nodejs prev.npm prev.makeWrapper];

    buildPhase = ''
      export HOME=$TMPDIR
      npm config set cache $TMPDIR/npm-cache
      npm install $src
    '';

    installPhase = ''
      mkdir -p $out/libexec $out/bin

      cp -r node_modules/openmemory-js $out/libexec/
      cp -r node_modules/.bin $out/libexec/.bin 2>/dev/null || true

      for dir in $out/libexec/node_modules/*/.bin; do
        [ -d "$dir" ] && cp -r $dir/* $out/libexec/.bin/ 2>/dev/null || true
      done

      makeWrapper ${prev.nodejs}/bin/node $out/bin/openmemory-js \
        --add-flags "$out/libexec/openmemory-js/dist/cli.js"

      chmod +x $out/libexec/openmemory-js/dist/cli.js 2>/dev/null || true
    '';

    passthru.updateScript = prev.writeShellScript "update-openmemory-js" ''
      set -euo pipefail

      # Get latest version
      LATEST=$(curl -sL https://registry.npmjs.org/openmemory-js/latest | jq -r .version)

      echo "Latest version: $LATEST"

      # Calculate new hash first
      URL="https://registry.npmjs.org/openmemory-js/-/openmemory-js-$LATEST.tgz"
      HASH=$(nix-prefetch-url "$URL")

      echo "Calculated hash: $HASH"

      # Update version in file
      sed -i '/pname = "openmemory-js"/,/^  };/ s|^    version = ".*"|    version = "'$LATEST'"|' modules/overlays/default.nix

      # Update hash in file
      sed -i '/pname = "openmemory-js"/,/^  };/ s|^      sha256 = ".*"|      sha256 = "'$HASH'"|' modules/overlays/default.nix

      echo "Updated openmemory-js to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "OpenMemory JavaScript SDK - Persistent memory for AI agents";
      homepage = "https://github.com/CaviraOSS/OpenMemory";
      license = licenses.mit;
      platforms = platforms.all;
    };
  };

  zed-editor = prev.stdenv.mkDerivation rec {
    pname = "zed-editor";
    version = "0.217.4";

    src = prev.fetchurl {
      url = "https://github.com/zed-industries/zed/releases/download/v${version}/zed-linux-x86_64.tar.gz";
      sha256 = "0i9915p5zqv9vwymjdl8cb2mjnak85q9z666s8q36g83j6amaml4";
    };

    nativeBuildInputs = [
      prev.autoPatchelfHook
      prev.makeWrapper
    ];

    buildInputs = with prev; [
      alsa-lib
      curl
      fontconfig
      freetype
      libGL
      libxkbcommon
      openssl
      stdenv.cc.cc.lib
      vulkan-loader
      wayland
      zlib
    ];

    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp -r * $out/

      # Overwrite the existing bin/zed with a symlink to libexec/zed-editor
      mkdir -p $out/bin
      ln -sf $out/libexec/zed-editor $out/bin/zed

      # Wrap the binary to include necessary libraries in LD_LIBRARY_PATH for dlopen
      wrapProgram $out/libexec/zed-editor \
        --prefix LD_LIBRARY_PATH : "${prev.lib.makeLibraryPath [
        prev.wayland
        prev.libxkbcommon
        prev.vulkan-loader
        prev.libGL
      ]}"

      runHook postInstall
    '';
    passthru.updateScript = prev.writeShellScript "update-zed" ''
      set -euo pipefail
      LATEST=$(curl -sL https://api.github.com/repos/zed-industries/zed/releases/latest | jq -r '.tag_name' | sed 's/^v//')
      echo "Latest version: $LATEST"

      # Calculate new hash first
      URL="https://github.com/zed-industries/zed/releases/download/v$LATEST/zed-linux-x86_64.tar.gz"
      HASH=$(nix-prefetch-url "$URL")
      echo "Calculated hash: $HASH"

      # Update version in file
      sed -i '/pname = "zed-editor"/,/^  };/ s|^    version = ".*"|    version = "'$LATEST'"|' modules/overlays/default.nix

      # Update hash in file
      sed -i '/pname = "zed-editor"/,/^  };/ s|^      sha256 = ".*"|      sha256 = "'$HASH'"|' modules/overlays/default.nix
      echo "Updated zed-editor to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "Zed Editor";
      homepage = "https://zed.dev";
      license = licenses.gpl3;
      platforms = platforms.linux;
    };
  };
}
