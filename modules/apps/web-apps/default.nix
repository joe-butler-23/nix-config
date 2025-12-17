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
      runHook postUnpack
    '';
    installPhase = "true";
  };

  # Define extension suite for reuse
  privacy-extensions = [
    ublock-origin
    clearurls
    consent-o-matic
    fastforward
  ];

  # Common flags for standalone app mode to suppress extension popups
  standalone-app-flags = "--no-first-run --no-default-browser-check --disable-background-mode --disable-extensions-file-access-check --disable-background-timer-throttling --disable-renderer-backgrounding";

  # Create wrapper scripts for each web app
  makeWebAppWrapper = name: url: dataDir: extraFlags: let
    extensionsList = extraFlags ++ privacy-extensions;
  in
    pkgs.writeShellScript "${name}-wrapper" ''
      # Create profile directory and preferences if not exists
      mkdir -p "${config.xdg.dataHome}/${dataDir}/Default"
      if [ ! -f "${config.xdg.dataHome}/${dataDir}/Default/Preferences" ]; then
        echo '{"profile":{"name":"Web App Profile"},"extensions":{"settings":{"pkehgijcmpdhfbdbbnkijodmdjhbjlgp":{"has_run":true},"aemomkdncapdnfajjbbcbdebjljbpmpj":{"firstRun":false,"firstrun":true,"skipWelcome":true}}},"first_run":false,"welcome_page":""}' > "${config.xdg.dataHome}/${dataDir}/Default/Preferences"
      fi

      # Launch the application
      exec ${pkgs.vivaldi}/bin/vivaldi ${standalone-app-flags} --app=${url} --load-extension=${lib.concatStringsSep "," extensionsList} --user-data-dir=${config.xdg.dataHome}/${dataDir}
    '';
in {
  xdg.desktopEntries = {
    chatgpt = {
      name = "ChatGPT";
      comment = "ChatGPT AI Assistant Web App";
      icon = "${config.home.homeDirectory}/nix-config/modules/apps/web-apps/icons/chatgpt.png";
      exec = "${makeWebAppWrapper "chatgpt" "https://chat.openai.com" "vivaldi-chatgpt" []}";
      categories = ["Development"];
      terminal = false;
    };

    spotify-web = {
      name = "Spotify Web Player";
      comment = "Spotify Web App";
      icon = "spotify";
      exec = "${makeWebAppWrapper "spotify" "https://open.spotify.com" "vivaldi-spotify" []}";
      categories = ["AudioVideo" "Audio"];
      terminal = false;
    };
    mathacademy = {
      name = "Math Academy";
      comment = "Math Academy Web App";
      icon = "${config.home.homeDirectory}/nix-config/modules/apps/web-apps/icons/mathacademy.png";
      exec = "${makeWebAppWrapper "mathacademy" "https://www.mathacademy.com/learn" "vivaldi-mathacademy" []}";
      categories = ["Education" "Science"];
      terminal = false;
    };

    youtube = {
      name = "YouTube";
      comment = "YouTube Web App";
      icon = "youtube";
      exec = "${pkgs.writeShellScript "youtube-wrapper" ''
        # Create profile directory and preferences if not exists
        mkdir -p "${config.xdg.dataHome}/brave-youtube/Default"
        if [ ! -f "${config.xdg.dataHome}/brave-youtube/Default/Preferences" ]; then
          echo '{"profile":{"name":"YouTube Web App"},"first_run":false,"welcome_page":""}' > "${config.xdg.dataHome}/brave-youtube/Default/Preferences"
        fi

        # Launch YouTube as windowed app with extensions for restrictions and YouTube-specific features
        # TEMPORARILY DISABLED app-locker to access extension settings
        exec ${pkgs.brave}/bin/brave \
          --app=https://www.youtube.com \
          --load-extension=${unhook-extension} \
          --user-data-dir=${config.xdg.dataHome}/brave-youtube \
          --disable-features=TranslateUI \
          --hide-scrollbars \
          --disable-background-timer-throttling \
          --disable-renderer-backgrounding \
          --no-first-run \
          --disable-default-apps
      ''}";
      categories = ["AudioVideo" "Video"];
      terminal = false;
    };

    # Override to hide the main Vivaldi browser entry from Rofi and other launchers.
    vivaldi = {
      name = "Vivaldi";
      exec = "${pkgs.vivaldi}/bin/vivaldi";
      noDisplay = true;
      terminal = false;
    };

    outlook = {
      name = "Outlook";
      comment = "Outlook Web App";
      icon = "${config.home.homeDirectory}/nix-config/modules/apps/web-apps/icons/outlook.png";
      exec = "${pkgs.writeShellScript "outlook-wrapper" ''
        # Create profile directory and preferences if not exists
        mkdir -p "${config.xdg.dataHome}/brave-outlook/Default"
        if [ ! -f "${config.xdg.dataHome}/brave-outlook/Default/Preferences" ]; then
          echo '{"profile":{"name":"Outlook Web App"},"extensions":{"settings":{"pkehgijcmpdhfbdbbnkijodmdjhbjlgp":{"has_run":true},"aemomkdncapdnfajjbbcbdebjljbpmpj":{"firstRun":false,"firstrun":true,"skipWelcome":true}}},"first_run":false,"welcome_page":""}' > "${config.xdg.dataHome}/brave-outlook/Default/Preferences"
        fi

        # Launch Outlook as Brave web app with privacy extensions
        exec ${pkgs.brave}/bin/brave --app=https://outlook.office.com/mail/ --load-extension=${lib.concatStringsSep "," privacy-extensions} --user-data-dir=${config.xdg.dataHome}/brave-outlook
      ''}";
      categories = ["Office" "Email"];
      terminal = false;
    };
  };
}
