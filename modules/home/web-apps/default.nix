{ pkgs, ... }: {
  xdg.desktopEntries = {
    chatgpt = {
      name = "ChatGPT";
      comment = "ChatGPT AI Assistant Web App";
      icon = "chatgpt";
      exec = "${pkgs.brave}/bin/brave --app=https://chat.openai.com";
      categories = [ "Network" "Office" "Development" ];
      terminal = false;
    };

    gmail = {
      name = "Gmail";
      comment = "Gmail Web App";
      icon = "gmail";
      exec = "${pkgs.brave}/bin/brave --app=https://gmail.com";
      categories = [ "Network" "Email" ];
      terminal = false;
    };

    spotify-web = {
      name = "Spotify Web Player";
      comment = "Spotify Web App";
      icon = "spotify";
      exec = "${pkgs.brave}/bin/brave --app=https://open.spotify.com";
      categories = [ "AudioVideo" "Audio" ];
      terminal = false;
    };

    twitter = {
      name = "Twitter";
      comment = "Twitter Web App";
      icon = "twitter";
      exec = "${pkgs.brave}/bin/brave --app=https://twitter.com";
      categories = [ "Network" "SocialMedia" ];
      terminal = false;
    };

    notion = {
      name = "Notion";
      comment = "Notion Web App";
      icon = "notion";
      exec = "${pkgs.brave}/bin/brave --app=https://notion.so";
      categories = [ "Office" "Development" ];
      terminal = false;
    };
  };
}
