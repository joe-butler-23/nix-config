{
  pkgs,
  config,
  ...
}: let
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
in {
  xdg.desktopEntries = {
    chatgpt = {
      name = "ChatGPT";
      comment = "ChatGPT AI Assistant Web App";
      icon = "${config.home.homeDirectory}/nix-config/modules/apps/web-apps/icons/chatgpt.png";
      exec = "${pkgs.vivaldi}/bin/vivaldi --app=https://chat.openai.com";
      categories = ["Development"];
      terminal = false;
    };

    spotify-web = {
      name = "Spotify Web Player";
      comment = "Spotify Web App";
      icon = "spotify";
      exec = "${pkgs.vivaldi}/bin/vivaldi --app=https://open.spotify.com";
      categories = ["AudioVideo" "Audio"];
      terminal = false;
    };

    mathacademy = {
      name = "Math Academy";
      comment = "Math Academy Web App";
      icon = "${config.home.homeDirectory}/nix-config/modules/apps/web-apps/icons/mathacademy.png";
      exec = "${pkgs.vivaldi}/bin/vivaldi --app=https://www.mathacademy.com/learn";
      categories = ["Education" "Science"];
      terminal = false;
    };

    youtube = {
      name = "YouTube";
      comment = "YouTube Web App (Unhooked)";
      icon = "youtube";
      exec = "${pkgs.vivaldi}/bin/vivaldi --app=https://www.youtube.com --load-extension=${unhook-extension} --user-data-dir=${config.xdg.dataHome}/vivaldi-youtube-unhooked";
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
      icon = "microsoft-outlook"; # Assuming a standard icon name might exist or be handled by the system
      exec = "${pkgs.vivaldi}/bin/vivaldi --app=https://outlook.office.com/mail/";
      categories = ["Office" "Email"];
      terminal = false;
    };
  };
}
