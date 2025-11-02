{ config, pkgs, ... }:

{
  programs.zsh = {
    enable = true;

    # Shell options
    defaultKeymap = "emacs";
    
    # History configuration
    history = {
      size = 10000;
      save = 10000;
      path = "${config.xdg.dataHome}/zsh/history";
      ignoreAllDups = true;
      ignoreSpace = true;
      share = false;
      extended = false;
    };

    # Enable plugins via home-manager
    syntaxHighlighting = {
      enable = true;
    };

    autosuggestion = {
      enable = true;
      strategy = ["completion" "history"];
    };

    historySubstringSearch = {
      enable = true;
    };

    # Shell aliases
    shellAliases = {
      k = "kitty @ launch --type=os-window";
      nvim = "nvim --listen /tmp/nvim";
    };

    # Completion initialization
    enableCompletion = true;
    completionInit = ''
      # Lazy completion initialization
      mkdir -p "$HOME/.zsh/cache"

      _lazy_compinit() {
        unfunction _lazy_compinit 2>/dev/null
        autoload -Uz compinit
        compinit -C -d "$HOME/.zsh/cache/zcompdump-$ZSH_VERSION"
        if [[ -r "$HOME/.zsh/cache/zcompdump-$ZSH_VERSION" && ! -r "$HOME/.zsh/cache/zcompdump-$ZSH_VERSION.zwc" ]]; then
          zcompile "$HOME/.zsh/cache/zcompdump-$ZSH_VERSION"
        fi
        bindkey '^I' complete-word
        zle complete-word
      }
      zle -N _lazy_compinit
      bindkey '^I' _lazy_compinit
    '';

    # Interactive shell initialization
    initExtra = ''
      ####################
      # Options
      ####################
      setopt correct extendedglob nocaseglob rcexpandparam nocheckjobs \
             numericglobsort nobeep appendhistory histignorealldups \
             autocd inc_append_history histignorespace interactivecomments

      export TERM=xterm-256color
      export COLORTERM=truecolor

      ####################
      # 1Password Injection
      ####################
      if command -v op >/dev/null 2>&1 && [ -S "/run/user/$UID/1password/agent.sock" ]; then
        if timeout 1s op account list >/dev/null 2>&1; then
          eval "$(op inject --in-file "$HOME/.dotfiles/secrets.zsh")"
        fi
      fi

      ####################
      # Autosuggestions Strategy
      ####################
      ZSH_AUTOSUGGEST_STRATEGY=(completion history)

      ####################
      # Completion Configuration
      ####################
      zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
      zstyle ':completion:*' list-colors "''${(s.:.)LS_COLORS}"
      zstyle ':completion:*' rehash true
      zstyle ':completion:*' menu select
      zstyle ':completion:*' accept-exact '*(N)'
      zstyle ':completion:*' use-cache on
      zstyle ':completion:*' cache-path "$HOME/.zsh/cache"

      ############################
      # Yazi chooser + cd/open fn
      ############################
      y() {
        local tmp_cwd tmp_pick cwd pick
        tmp_cwd=$(mktemp -t "yazi-cwd.XXXXXX")
        tmp_pick=$(mktemp -t "yazi-pick.XXXXXX")
        yazi "$@" --cwd-file "$tmp_cwd" --chooser-file "$tmp_pick"
        pick=$(<"$tmp_pick")
        if [ -n "$pick" ]; then
          nvim "$pick"
        else
          cwd=$(<"$tmp_cwd")
          if [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
            builtin cd -- "$cwd"
          fi
        fi
        rm -f -- "$tmp_cwd" "$tmp_pick"
      }

      #############
      # Key binds
      #############
      bindkey -s "^[Q" "clear && printf '\e[3J'\n"

      #############
      # Lazy loading
      #############

      # ---- Lazy-load Node/NVM ----
      if [ -d "$HOME/.nvm" ]; then
        _lazy_nvm_bootstrap() {
          unfunction _lazy_nvm_bootstrap 2>/dev/null
          unset -f node npm npx corepack yarn pnpm 2>/dev/null
          export NVM_DIR="$HOME/.nvm"
          [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
        }
        node()     { _lazy_nvm_bootstrap; command node "$@"; }
        npm()      { _lazy_nvm_bootstrap; command npm "$@"; }
        npx()      { _lazy_nvm_bootstrap; command npx "$@"; }
        corepack() { _lazy_nvm_bootstrap; command corepack "$@"; }
        yarn()     { _lazy_nvm_bootstrap; command yarn "$@"; }
        pnpm()     { _lazy_nvm_bootstrap; command pnpm "$@"; }
      fi

      # ---- Lazy-load Conda ----
      if [ -f "$HOME/anaconda3/etc/profile.d/conda.sh" ] || [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
        _lazy_conda_bootstrap() {
          unfunction _lazy_conda_bootstrap 2>/dev/null
          unset -f conda activate 2>/dev/null
          if [ -f "$HOME/anaconda3/etc/profile.d/conda.sh" ]; then
            . "$HOME/anaconda3/etc/profile.d/conda.sh"
          elif [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
            . "$HOME/miniconda3/etc/profile.d/conda.sh"
          fi
        }
        conda()    { _lazy_conda_bootstrap; command conda "$@"; }
        activate() { _lazy_conda_bootstrap; command activate "$@"; }
      fi

      ############################################
      # Copy last terminal output
      ############################################
      # Clipboard helper (Wayland/X11/mac/OSC52 fallback)
      to_clipboard() {
        if command -v wl-copy >/dev/null 2>&1; then wl-copy -n
        elif command -v xclip >/dev/null 2>&1; then xclip -selection clipboard
        elif command -v pbcopy >/dev/null 2>&1; then pbcopy
        else
          local data
          data=$(base64 -w0 2>/dev/null || base64 | tr -d '\n')
          printf '\033]52;c;%s\007' "$data"
        fi
      }

      # Remember last command exactly as it will be run
      preexec() { LAST_CMD="$ZSH_COMMAND"; }

      # Re-run last command, mirror to screen, copy full output
      copy_last_output() {
        if [ -z "$LAST_CMD" ]; then
          zle -M "No previous command"
          return 1
        fi

        local tmp status
        tmp=$(mktemp) || return 1
        { eval -- "$LAST_CMD" } > >(tee "$tmp") 2> >(tee -a "$tmp" >&2)
        status=$?
        to_clipboard < "$tmp"
        rm -f -- "$tmp"
        return $status
      }

      zle -N copy_last_output
      bindkey '^[c' copy_last_output
    '';
  };

  # Zoxide integration
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
}
