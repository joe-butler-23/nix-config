{config, ...}: {
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    documents = "${config.home.homeDirectory}/documents";
    download = "${config.home.homeDirectory}/downloads";
    pictures = "${config.home.homeDirectory}/pictures";
    desktop = "${config.home.homeDirectory}/desktop";

    # Point unwanted directories to $HOME to prevent their creation
    music = "${config.home.homeDirectory}";
    videos = "${config.home.homeDirectory}";
    publicShare = "${config.home.homeDirectory}";
    templates = "${config.home.homeDirectory}";
  };
}
