{
  config,
  user,
  lib,
  pkgs,
  ...
}: {
  programs.zsh = {
    enable = true;

    ############################
    # Powerlevel10k theme (plugin)
    ############################
    plugins = [
      {
        name = "powerlevel10k";
        src = "${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k";
        file = "powerlevel10k.zsh-theme";
      }
    ];

    ############################
    # History configuration
    ############################
    history = {
      size = 10000;
      save = 10000;
      path = "${config.xdg.dataHome}/zsh/history";
      ignoreAllDups = true;
      ignoreSpace = true;
      share = false;
      extended = false;
    };

    ############################
    # Built-in plugin helpers
    ############################
    syntaxHighlighting.enable = true;

    autosuggestion = {
      enable = true;
      strategy = ["completion" "history"];
    };

    historySubstringSearch.enable = true;

    ############################
    # Shell aliases
    ############################
    shellAliases = {
      hs = "home-manager switch --flake \"$HOME/nix-config#${user}\"";
      ns = "sudo nixos-rebuild switch --flake \"$HOME/nix-config\"";
      hsdry = "home-manager build --flake \"$HOME/nix-config#${user}\"";
      nsdry = "sudo nixos-rebuild dry-build --flake \"$HOME/nix-config\"";
      hn = "home-manager news";
      hstatus = "home-manager generations";
    };

    ############################
    # Completion initialisation
    ############################
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

    ############################
    # Main interactive init
    ############################
    initContent = lib.mkBefore ''
      ####################
      # CRITICAL: Set PROMPT_EOL_MARK before ANYTHING else
      # This prevents blue tilde artifacts on resize
      ####################
      PROMPT_EOL_MARK=""

      ############################
      # Powerlevel10k Instant Prompt
      # Must come before any console output
      ############################
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi

      ####################
      # Options
      ####################
      setopt correct extendedglob nocaseglob rcexpandparam nocheckjobs \
             numericglobsort nobeep appendhistory histignorealldups \
             autocd inc_append_history histignorespace interactivecomments

      ####################
      # Handle terminal resize - clear screen to prevent artifacts
      ####################
      TRAPWINCH() {
        # Clear screen and scrollback on resize to prevent ghost characters
        printf '\e[2J\e[3J\e[H'
        # Let zsh redraw the prompt
        zle && zle reset-prompt
      }

      ####################
      # 1Password Injection
      ####################
      if command -v op >/dev/null 2>&1 && [ -S "/run/user/$UID/1password/agent.sock" ]; then
        if timeout 1s op account list >/dev/null 2>&1; then
          eval "$(op inject --in-file "$HOME/.dotfiles/secrets.zsh")"
        fi
      fi

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
      # Foot: mark start/end of command output (OSC-133)
      ############################################
      autoload -Uz add-zsh-hook

      foot_precmd() {
        if ! builtin zle; then
          print -n "\e]133;D\e\\"
        fi
      }

      foot_preexec() {
        print -n "\e]133;C\e\\"
        LAST_CMD="$ZSH_COMMAND"
      }

      add-zsh-hook precmd foot_precmd
      add-zsh-hook preexec foot_preexec

      ####################
      # Powerlevel10k user config
      ####################
      if [[ -r "$HOME/.p10k.zsh" ]]; then
        source "$HOME/.p10k.zsh"
      fi
    '';
  };

  ############################
  # Zoxide integration
  ############################
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  ############################
  # Declarative file management
  ############################
  home.file.".p10k.zsh".source = ./p10k.zsh;
}
