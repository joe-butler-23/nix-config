{
  user,
  pkgs,
  ...
}: let
  userHome = "/home/${user}";
in {
  environment.etc."zsh_plugin_paths".text = ''
    export ZSH_POWERLEVEL10K_PATH="${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme"
    export ZSH_AUTOSUGGESTIONS_PATH="${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
    export ZSH_SYNTAX_HIGHLIGHTING_PATH="${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
    export ZSH_HISTORY_SUBSTRING_SEARCH_PATH="${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
  '';

  environment.sessionVariables = {
    CARGO_HOME = "${userHome}/.local/share/rust/cargo";
    RUSTUP_HOME = "${userHome}/.local/share/rust/rustup";

    BUN_INSTALL = "${userHome}/.local/share/bun";
    npm_config_cache = "${userHome}/.cache/npm";
    NPM_CONFIG_USERCONFIG = "${userHome}/.config/npm/npmrc";

    SOPS_AGE_KEY_FILE = "${userHome}/nix-config/secrets/sops.agekey";

    EDITOR = "emacsclient -c -n";
    VISUAL = "emacsclient -c -n";
    SOPS_EDITOR = "emacsclient -t";

    XDG_CONFIG_HOME = "${userHome}/.config";
    XDG_CACHE_HOME = "${userHome}/.cache";
    XDG_DATA_HOME = "${userHome}/.local/share";
    XDG_STATE_HOME = "${userHome}/.local/state";
  };
}
