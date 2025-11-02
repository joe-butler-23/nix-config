_: {
  services.mako = {
    enable = true;

    settings = {
      # Theme settings - commented out to allow Stylix theming
      # font = "JetBrainsMono Nerd Font 11";
      # background-color = "#1e1e2e";
      # text-color = "#cdd6f4";
      # border-color = "#585b70";
      # progress-color = "#89b4fa";
      border-size = 2;
      border-radius = 8;
      width = 350;
      height = 120;
      max-visible = 5;
      default-timeout = 5000;
      margin = "10,10,0,0";
      padding = "8,10";
      layer = "overlay";
      anchor = "top-right";
    };
  };
}
