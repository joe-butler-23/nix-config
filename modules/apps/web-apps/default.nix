{
  pkgs,
  config,
  lib,
  ...
}: let
  # Helper to create a first-run preferences file to suppress extension popups
  unhook-extension = pkgs.stdenv.mkDerivation {
    name = "unhook-extension";
    src = pkgs.fetchurl {
      url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=120.0&x=id%3Dkhncfooichmfjbepaaaebmommgaepoid%26uc";
      name = "unhook.crx";
      sha256 = "1wwz47zb11ybgi025pbwi911g3ddzv0pkvgybgddxdnjp874xs4f";
    };
    nativeBuildInputs = [pkgs.unzip];
    unpackPhase = ''
      runHook preUnpack
      unzip $src -d $out || true
      sed -i 's|tabs.create({url:"https://unhook.app/welcome"})|void(0)|g' $out/js/background.js
      runHook postUnpack
    '';
    installPhase = "true";
  };

  # Simple extension to make web apps more app-like by disabling context menus

  ublock-origin = pkgs.stdenv.mkDerivation {
    name = "ublock-origin";
    src = pkgs.fetchurl {
      url = "https://github.com/gorhill/uBlock/releases/download/1.68.0/uBlock0_1.68.0.chromium.zip";
      sha256 = "1qqh409vzls9rzab945mp0r516sjv1z11zh0sbsp2vlhvl4gp5ln";
    };
    nativeBuildInputs = [pkgs.libarchive];
    unpackPhase = ''
      runHook preUnpack
      mkdir -p $out
      bsdtar -xf $src -C $out --strip-components=1
      # Prevent uBlock Origin from opening the dashboard/first-run page by logging instead of opening
      find $out -name "*.js" -exec sed -i "s|µb.openNewTab({url:'dashboard.html'|console.log({url:'dashboard.html'|g" {} +
      runHook postUnpack
    '';
    installPhase = "true";
  };

  clearurls = pkgs.stdenv.mkDerivation {
    name = "clearurls";
    src = pkgs.fetchurl {
      url = "https://github.com/ClearURLs/Addon/releases/download/1.27.3/ClearURLs.zip";
      sha256 = "0j6iaqan18jdpwvmdvfq0ks4x24l99dvvd7v1wmmd3vx7sfqfp1d";
    };
    nativeBuildInputs = [pkgs.libarchive];
    unpackPhase = ''
      runHook preUnpack
      mkdir -p $out
      bsdtar -xf $src -C $out
      runHook postUnpack
    '';
    installPhase = "true";
  };

  consent-o-matic = pkgs.stdenv.mkDerivation {
    name = "consent-o-matic";
    src = pkgs.fetchurl {
      url = "https://github.com/cavi-au/Consent-O-Matic/releases/download/v1.1.5/consent-o-matic-v1.1.5-unpacked-release-chromium.zip";
      sha256 = "0fj7l5xqhx2wmhcn4mfzp030pzvrrc0nm7g3352chlszzj0wikip";
    };
    nativeBuildInputs = [pkgs.libarchive];
    unpackPhase = ''
      runHook preUnpack
      mkdir -p $out
      # Create a parent directory to match expected Chromium extension structure
      bsdtar -xf $src -C $out --strip-components=0
      # Prevent Consent-O-Matic from opening options page on install by logging instead of creating tab
      find $out -name "*.js" -exec sed -i 's|chrome.tabs.create({url:chrome.runtime.getURL("options.html")|console.log({url:chrome.runtime.getURL("options.html")|g' {} +
      runHook postUnpack
    '';
    installPhase = "true";
  };

  fastforward = pkgs.stdenv.mkDerivation {
    name = "fastforward";
    src = pkgs.fetchurl {
      url = "https://github.com/FastForwardTeam/FastForward/releases/download/manifest-v2-release/fastforward-chromium.zip";
      sha256 = "1smms20nhx7crbjdh9ckmjg1lc6i0zriy8hwz3jfbapzmlkkz63q";
    };
    nativeBuildInputs = [pkgs.libarchive];
    unpackPhase = ''
      runHook preUnpack
      mkdir -p $out
      # The zip already contains a top-level directory, so don't strip components
      bsdtar -xf $src -C $out --strip-components=0
      # Prevent FastForward from opening the first-run page by logging instead
      find $out -name "*.js" -exec sed -i 's|brws.tabs.create({url:"https://fastforward.team|console.log({url:"https://fastforward.team|g' {} +
      runHook postUnpack
    '';
    installPhase = "true";
  };

  # Define extension suite for reuse (combining general privacy + app specific)
  all-extensions = [
    ublock-origin
    clearurls
    consent-o-matic
    fastforward
    unhook-extension
  ];

  # Common flags for standalone app mode to suppress extension popups and improve speed
  standalone-app-flags = "--no-first-run --no-default-browser-check --disable-background-mode --disable-extensions-file-access-check --disable-background-timer-throttling --disable-renderer-backgrounding --disable-component-update --disable-sync --disable-breakpad --disable-crash-reporter --disable-speech-api --disable-domain-reliability --no-pings";

  # Shared profile directory for the "Zygote" strategy
  shared-profile-dir = "${config.xdg.dataHome}/brave-webapps";

  # Unified launcher script for all web apps
  webappLauncher = pkgs.writeShellScript "webapp-launcher" ''
    APP_URL="$1"
    PROFILE_DIR="${shared-profile-dir}"
    EXTENSIONS="${lib.concatStringsSep "," all-extensions}"

    # Ensure profile and preferences exist
    mkdir -p "$PROFILE_DIR/Default"
    if [ ! -f "$PROFILE_DIR/Default/Preferences" ]; then
      # Initialize preferences to suppress extension popups (using IDs found in previous configs)
      echo '{"profile":{"name":"Web Apps"},"extensions":{"settings":{"pkehgijcmpdhfbdbbnkijodmdjhbjlgp":{"has_run":true},"aemomkdncapdnfajjbbcbdebjljbpmpj":{"firstRun":false,"firstrun":true,"skipWelcome":true}}},"first_run":false,"welcome_page":""}' > "$PROFILE_DIR/Default/Preferences"
    fi

    # Launch the web app using the shared profile
    # If the browser is already running (Zygote), this opens a new window instantly.
    exec ${pkgs.brave}/bin/brave ${standalone-app-flags} \
      --app="$APP_URL" \
      --load-extension="$EXTENSIONS" \
      --user-data-dir="$PROFILE_DIR"
  '';
in {
  xdg.desktopEntries = {
    chatgpt = {
      name = "ChatGPT";
      comment = "ChatGPT AI Assistant Web App";
      icon = "${config.home.homeDirectory}/nix-config/modules/apps/web-apps/icons/chatgpt.png";
      exec = "${webappLauncher} https://chat.openai.com";
      categories = ["Development"];
      terminal = false;
    };

    spotify-web = {
      name = "Spotify Web Player";
      comment = "Spotify Web App";
      icon = "spotify";
      exec = "${webappLauncher} https://open.spotify.com";
      categories = ["AudioVideo" "Audio"];
      terminal = false;
    };

    mathacademy = {
      name = "Math Academy";
      comment = "Math Academy Web App";
      icon = "${config.home.homeDirectory}/nix-config/modules/apps/web-apps/icons/mathacademy.png";
      exec = "${webappLauncher} https://www.mathacademy.com/learn";
      categories = ["Education" "Science"];
      terminal = false;
    };

    youtube = {
      name = "YouTube";
      comment = "YouTube Web App";
      icon = "youtube";
      exec = "${webappLauncher} https://www.youtube.com";
      categories = ["AudioVideo" "Video"];
      terminal = false;
    };

    outlook = {
      name = "Outlook";
      comment = "Outlook Web App";
      icon = "${config.home.homeDirectory}/nix-config/modules/apps/web-apps/icons/outlook.png";
      exec = "${webappLauncher} https://outlook.office.com/mail/";
      categories = ["Office" "Email"];
      terminal = false;
    };

    # Utility to configure extensions for the shared profile
    configure-webapps = {
      name = "Configure Web Apps";
      comment = "Open Shared Web Apps profile to configure extensions";
      icon = "preferences-system";
      exec = "${pkgs.writeShellScript "configure-webapps-wrapper" ''
        exec ${pkgs.brave}/bin/brave \
          about:blank \
          --load-extension="${lib.concatStringsSep "," all-extensions}" \
          --user-data-dir="${shared-profile-dir}" \
          --no-first-run
      ''}";
      categories = ["Settings"];
      terminal = false;
    };

    # Hide standard Vivaldi if unused
    vivaldi = {
      name = "Vivaldi";
      exec = "${pkgs.vivaldi}/bin/vivaldi";
      noDisplay = true;
      terminal = false;
    };
  };
}
