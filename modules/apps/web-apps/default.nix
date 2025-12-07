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
      exec = "${pkgs.brave}/bin/brave --app=https://chat.openai.com";
      categories = ["Development"];
      terminal = false;
    };

    gmail = {
      name = "Gmail";
      comment = "Gmail Web App";
      icon = "gmail";
      exec = "${pkgs.brave}/bin/brave --app=https://gmail.com";
      categories = ["Network" "Email"];
      terminal = false;
    };

    spotify-web = {
      name = "Spotify Web Player";
      comment = "Spotify Web App";
      icon = "spotify";
      exec = "${pkgs.brave}/bin/brave --app=https://open.spotify.com";
      categories = ["AudioVideo" "Audio"];
      terminal = false;
    };

    twitter = {
      name = "Twitter";
      comment = "Twitter Web App";
      icon = "twitter";
      exec = "${pkgs.brave}/bin/brave --app=https://twitter.com";
      categories = ["Network"];
      terminal = false;
    };

    notion = {
      name = "Notion";
      comment = "Notion Web App";
      icon = "notion";
      exec = "${pkgs.brave}/bin/brave --app=https://notion.so";
      categories = ["Office" "Development"];
      terminal = false;
    };

    mathacademy = {
      name = "Math Academy";
      comment = "Math Academy Web App";
      icon = "${config.home.homeDirectory}/nix-config/modules/apps/web-apps/icons/mathacademy.png";
      exec = "${pkgs.brave}/bin/brave --app=https://www.mathacademy.com/learn";
      categories = ["Education" "Science"];
      terminal = false;
    };
  };
}
