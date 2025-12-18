{config, ...}: {
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = "brave-gatekeeper.desktop";
      "x-scheme-handler/http" = "brave-gatekeeper.desktop";
      "x-scheme-handler/https" = "brave-gatekeeper.desktop";
      "x-scheme-handler/about" = "brave-gatekeeper.desktop";
      "x-scheme-handler/unknown" = "brave-gatekeeper.desktop";

      "application/pdf" = "org.pwmt.zathura.desktop";
      "application/x-pdf" = "org.pwmt.zathura.desktop";

      "image/jpeg" = "viewnior.desktop";
      "image/png" = "viewnior.desktop";
      "image/gif" = "viewnior.desktop";
      "image/webp" = "viewnior.desktop";
      "image/svg+xml" = "viewnior.desktop";

      "text/plain" = "emacsclient.desktop";
      "text/markdown" = "emacsclient.desktop";
      "text/x-tex" = "emacsclient.desktop";
      "text/x-nix" = "emacsclient.desktop";
      "text/x-shellscript" = "emacsclient.desktop";
      "application/x-shellscript" = "emacsclient.desktop";

      "inode/directory" = "thunar.desktop";
    };
  };

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

  home.sessionVariables = {
    CARGO_HOME = "${config.home.homeDirectory}/.local/share/rust/cargo";
    RUSTUP_HOME = "${config.home.homeDirectory}/.local/share/rust/rustup";

    # Node / Bun
    BUN_INSTALL = "${config.home.homeDirectory}/.local/share/bun";
    npm_config_cache = "${config.home.homeDirectory}/.cache/npm";
    NPM_CONFIG_USERCONFIG = "${config.xdg.configHome}/npm/npmrc";
  };
}
