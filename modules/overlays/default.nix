{inputs}: _final: prev: {
  opencode = prev.stdenv.mkDerivation rec {
    pname = "opencode";
    version = "1.0.110";

    src = prev.fetchurl {
      url = "https://github.com/sst/opencode/releases/download/v${version}/opencode-linux-x64.tar.gz";
      sha256 = "177p16snqf9zylxp6949arn5n19m10d1xqj0pmw9phjwmm28xj26";
    };

    nativeBuildInputs = [prev.autoPatchelfHook];

    # Runtime dependencies (libraries that the binary might need)
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

    meta = with prev.lib; {
      description = "OpenCode CLI";
      homepage = "https://github.com/sst/opencode";
      license = licenses.mit; # Assuming MIT, check repo
      platforms = platforms.linux;
    };
  };
}
