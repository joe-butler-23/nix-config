{pkgs, ...}: {
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;

    # Font - commented out to allow Stylix font management
    # font = "Mono 12";
    terminal = "rofi-sensible-terminal";

    extraConfig = {
      modi = "drun,run,filebrowser,window";
      case-sensitive = false;
      cycle = true;
      filter = "";
      scroll-method = 0;
      normalize-match = true;
      show-icons = true;
      icon-theme = "Papirus";
      disable-history = false;
      steal-focus = false;

      # Matching
      matching = "fuzzy";
      tokenize = true;

      # SSH
      ssh-client = "ssh";
      ssh-command = "{terminal} -e {ssh-client} {host} [-p {port}]";
      parse-hosts = true;
      parse-known-hosts = true;

      # Drun
      drun-match-fields = "name,generic,exec,categories,keywords";
      drun-display-format = "{name} [<span weight='light' size='small'><i>({generic})</i></span>]";
      drun-show-actions = false;
      drun-url-launcher = "xdg-open";
      drun-use-desktop-cache = false;
      drun-reload-desktop-cache = false;

      # Run
      run-command = "{cmd}";
      run-shell-command = "{terminal} -e {cmd}";

      # Window switcher
      window-match-fields = "title,class,role,name,desktop";
      window-command = "wmctrl -i -R {window}";
      window-format = "{w} - {c} - {t:0}";
      window-thumbnail = false;

      # History and sorting
      sorting-method = "normal";
      max-history-size = 25;

      # Display
      display-window = "Windows";
      display-windowcd = "Window CD";
      display-run = "Run";
      display-ssh = "SSH";
      display-drun = "Apps";
      display-combi = "Combi";
      display-keys = "Keys";
      display-filebrowser = "Files";

      # Misc
      sort = false;
      threads = 0;
      click-to-exit = true;
    };
  };
}
