{
  user,
  pkgs,
  ...
}: {
  # Write Zsh plugin paths to a file that can be sourced by .zshrc
  # This avoids the need to log out/in for sessionVariables to update
  environment.etc."zsh_plugin_paths".text = ''
    export ZSH_POWERLEVEL10K_PATH="${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme"
    export ZSH_AUTOSUGGESTIONS_PATH="${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
    export ZSH_SYNTAX_HIGHLIGHTING_PATH="${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
    export ZSH_HISTORY_SUBSTRING_SEARCH_PATH="${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
  '';

  # System-level environment variables
  environment.sessionVariables = {
    # Rust
    CARGO_HOME = "/home/${user}/.local/share/rust/cargo";
    RUSTUP_HOME = "/home/${user}/.local/share/rust/rustup";

    # Node / Bun
    BUN_INSTALL = "/home/${user}/.local/share/bun";
    npm_config_cache = "/home/${user}/.cache/npm";
    NPM_CONFIG_USERCONFIG = "/home/${user}/.config/npm/npmrc";

    # SOPS
    SOPS_AGE_KEY_FILE = "/home/${user}/nix-config/secrets/sops.agekey";

    # Emacs
    EDITOR = "emacsclient -c -n";
    VISUAL = "emacsclient -c -n";
    SOPS_EDITOR = "emacsclient -t";
    EMACS_NATIVE_COMP_ASYNC_REPORT_WARNINGS_ERRORS = "nil";
  };
}
