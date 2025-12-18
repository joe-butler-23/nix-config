_: {
  # Install Kitty package and enable shell integration
  # Config (settings, theme, keybindings) managed by chezmoi
  programs.kitty = {
    enable = true;
    shellIntegration.enableZshIntegration = true;
  };
}
