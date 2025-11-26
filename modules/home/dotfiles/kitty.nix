{ pkgs, ... }: {
  programs.kitty = {
    enable = true;
    
    settings = {
      # --- Fonts & UI ---
      font_family = "JetBrainsMono Nerd Font";
      font_size = "10.0";
      cursor_shape = "beam";
      scrollback_lines = 10000;
      enable_audio_bell = false;
      
      # Fixes the "ghost window" animation glitch on exit
      confirm_os_window_close = 0;

      # --- Nord Theme (Matched to your Foot config) ---
      background = "#2e3440";
      foreground = "#d8dee9";
      selection_background = "#d8dee9";
      selection_foreground = "#2e3440";
      url_color = "#88c0d0";

      # Black
      color0 = "#3b4252";
      color8 = "#4c566a";
      # Red
      color1 = "#bf616a";
      color9 = "#bf616a";
      # Green
      color2 = "#a3be8c";
      color10 = "#a3be8c";
      # Yellow
      color3 = "#ebcb8b";
      color11 = "#ebcb8b";
      # Blue
      color4 = "#81a1c1";
      color12 = "#81a1c1";
      # Magenta
      color5 = "#b48ead";
      color13 = "#b48ead";
      # Cyan
      color6 = "#88c0d0";
      color14 = "#8fbcbb";
      # White
      color7 = "#e5e9f0";
      color15 = "#eceff4";
    };

    # --- Shell Integration ---
    # This automatically injects the scripts needed to track command output
    shellIntegration.enableZshIntegration = true;

    # --- Keybindings ---
    keybindings = {
      # This mimics your Foot behavior:
      # It grabs the output of the last command and sends it to the clipboard.
      "ctrl+shift+b" = "launch --stdin-source=@last_cmd_output --type=clipboard"; 
      
      # I added Ctrl+Space as an alternative since you mentioned it in the text
      "ctrl+space" = "launch --stdin-source=@last_cmd_output --type=clipboard";
    };
  };
}