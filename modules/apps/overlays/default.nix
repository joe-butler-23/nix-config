# ========================================
# CUSTOM OVERLAYS
# ========================================
# This file acts as a "patch layer" on top of the standard Nixpkgs.
# It injects custom tools that are not available in the official repositories
# or where we need a specific version/fork.
#
# TOOLS PROVIDED:
# 1. opencode: The OpenCode CLI agent (https://github.com/sst/opencode)
# 2. gemini: The Google Gemini CLI (https://github.com/google-gemini/gemini-cli)
# 3. codex: The OpenAI Codex CLI (https://github.com/openai/codex)
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
    version = "1.0.141";

    src = prev.fetchurl {
      url = "https://github.com/sst/opencode/releases/download/v${version}/opencode-linux-x64.tar.gz";
      sha256 = "16sqzbiv2wyralrg979qdsn94hidwmgnhnxq0qna2vbbr28j4i42";
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
      LATEST=$(curl -s https://api.github.com/repos/sst/opencode/releases/latest | jq -r '.tag_name' | sed 's/^v//')

      echo "Latest version: $LATEST"

      # Update version in file
      sed -i "s/version = \".*\";/version = \"$LATEST\";/" modules/apps/overlays/default.nix

      # Calculate new hash
      URL="https://github.com/sst/opencode/releases/download/v$LATEST/opencode-linux-x64.tar.gz"
      HASH=$(nix-prefetch-url "$URL")

      # Update hash in file
      sed -i "s|sha256 = \".*\";|sha256 = \"$HASH\";|" modules/apps/overlays/default.nix

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
    version = "0.21.0-preview.0";

    src = prev.fetchurl {
      url = "https://github.com/google-gemini/gemini-cli/releases/download/v${version}/gemini.js";
      sha256 = "0ywb0xjqa7kyiqqyffg3vfn252v76wkvnppch4inj44nfm6v0bna";
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
      LATEST=$(curl -s https://api.github.com/repos/google-gemini/gemini-cli/releases/latest | jq -r '.tag_name' | sed 's/^v//')

      echo "Latest version: $LATEST"

      # Update version in file
      sed -i "s/version = \".*\";/version = \"$LATEST\";/" modules/apps/overlays/default.nix

      # Calculate new hash
      URL="https://github.com/google-gemini/gemini-cli/releases/download/v$LATEST/gemini.js"
      HASH=$(nix-prefetch-url "$URL")

      # Update hash in file
      sed -i "s|sha256 = \".*\";|sha256 = \"$HASH\";|" modules/apps/overlays/default.nix

      echo "Updated gemini-cli to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "Gemini CLI";
      homepage = "https://github.com/google-gemini/gemini-cli";
      license = licenses.asl20;
      platforms = platforms.all;
    };
  };

  codex = prev.stdenv.mkDerivation rec {
    pname = "codex";
    version = "0.71.0";

    src = prev.fetchurl {
      url = "https://github.com/openai/codex/releases/download/rust-v${version}/codex-x86_64-unknown-linux-gnu.tar.gz";
      sha256 = "1y35ywhchrj5g18k8n41f4wy7y4nkj5q6hdgzpxqh8cr8hhy8dqf";
    };

    nativeBuildInputs = [prev.autoPatchelfHook];

    buildInputs = [
      prev.stdenv.cc.cc.lib
      prev.zlib
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
      LATEST=$(curl -s https://api.github.com/repos/openai/codex/releases/latest | jq -r '.tag_name' | sed 's/^rust-v//')

      echo "Latest version: $LATEST"

      # Update version in file
      sed -i "s/version = \".*\";/version = \"$LATEST\";/" modules/apps/overlays/default.nix

      # Calculate new hash
      URL="https://github.com/openai/codex/releases/download/rust-v$LATEST/codex-x86_64-unknown-linux-gnu.tar.gz"
      HASH=$(nix-prefetch-url "$URL")

      # Update hash in file
      sed -i "s|sha256 = \".*\";|sha256 = \"$HASH\";|" modules/apps/overlays/default.nix

      echo "Updated codex to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "Codex CLI";
      homepage = "https://github.com/openai/codex";
      license = licenses.asl20;
      platforms = platforms.linux;
    };
  };
}
