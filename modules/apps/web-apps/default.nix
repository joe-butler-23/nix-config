{
  pkgs,
  config,
  ...
}: {
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

    # Override to hide the main Vivaldi browser entry from Rofi and other launchers.
    vivaldi = {
      name = "Vivaldi";
      exec = "${pkgs.vivaldi}/bin/vivaldi";
      noDisplay = true;
      terminal = false;
    };
  };
}
