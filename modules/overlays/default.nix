{inputs}: final: prev: {
  opencode = prev.stdenv.mkDerivation rec {
    pname = "opencode";
    version = "1.0.110";

    src = prev.fetchurl {
      url = "https://github.com/sst/opencode/releases/download/v${version}/opencode-linux-x64.tar.gz";
      sha256 = "177p16snqf9zylxp6949arn5n19m10d1xqj0pmw9phjwmm28xj26";
    };

    nativeBuildInputs = [ prev.autoPatchelfHook ];
    
    # Runtime dependencies
    buildInputs = [ 
      prev.stdenv.cc.cc.lib 
      prev.zlib
    ];

    sourceRoot = ".";

    installPhase = ''
      runHook preInstall
      install -Dm755 opencode $out/bin/opencode
      runHook postInstall
    '';

    passthru.updateScript = prev.writeShellScript "update-opencode" ''
      set -euo pipefail
      
      # Get latest version
      LATEST=$(curl -s https://api.github.com/repos/sst/opencode/releases/latest | jq -r '.tag_name' | sed 's/^v//')
      
      echo "Latest version: $LATEST"
      
      # Update version in file
      sed -i "s/version = \".*\";/version = \"$LATEST\";/" modules/overlays/default.nix
      
      # Calculate new hash
      URL="https://github.com/sst/opencode/releases/download/v$LATEST/opencode-linux-x64.tar.gz"
      HASH=$(nix-prefetch-url "$URL")
      
      # Update hash in file
      sed -i "s|sha256 = \".*\";|sha256 = \"$HASH\";|" modules/overlays/default.nix
      
      echo "Updated opencode to $LATEST with hash $HASH"
    '';

    meta = with prev.lib; {
      description = "OpenCode CLI";
      homepage = "https://github.com/sst/opencode";
      license = licenses.mit; 
      platforms = platforms.linux;
    };
  };
}