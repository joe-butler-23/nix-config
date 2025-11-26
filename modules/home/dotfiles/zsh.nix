{
  config,
  user,
  lib,
  ...
}: {
  programs.zsh = {
    enable = true;

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

    # Main interactive init
    initContent = lib.mkOrder 1200 ''
      # Core modules / hooks
      autoload -Uz add-zsh-hook

      # minimal prompt
      PROMPT=$'\n%B%~%b > '

      # options
      setopt \
        correct \
        extendedglob \
        nocaseglob \
        rcexpandparam \
        nocheckjobs \
        numericglobsort \
        nobeep \
        appendhistory \
        histignorealldups \
        autocd \
        inc_append_history \
        histignorespace \
        interactivecomments

      # 1Password injection helper
      auth() {
        if command -v op >/dev/null 2>&1 && [ -S "/run/user/$UID/1password/agent.sock" ]; then
          echo "Injecting 1Password secrets..."
          eval "$(op inject --in-file "$HOME/.dotfiles/secrets.zsh")"
          echo "Secrets injected."
        else
          echo "Error: 1Password CLI (op) not found or agent socket missing."
        fi
      }

      # Yazi chooser + cd/open helper
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

      # Lazy completion (fast startup)

      # Ensure cache directory exists
      [[ -d "$HOME/.zsh/cache" ]] || mkdir -p "$HOME/.zsh/cache"

      _lazy_compinit() {
        # Only run once
        unfunction _lazy_compinit 2>/dev/null

        autoload -Uz compinit

        local zcompdump="$HOME/.zsh/cache/zcompdump-$ZSH_VERSION"

        # Use compiled dump if available and recompile in background if needed
        if [[ -s "$zcompdump" && ( ! -s "$zcompdump.zwc" || "$zcompdump" -nt "$zcompdump.zwc" ) ]]; then
          zcompile "$zcompdump" &!
        fi

        compinit -C -d "$zcompdump"

        # Completion styles
        zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
        zstyle ':completion:*' list-colors "''${(s.:.)LS_COLORS}"
        zstyle ':completion:*' rehash true
        zstyle ':completion:*' menu select
        zstyle ':completion:*' accept-exact '*(N)'
        zstyle ':completion:*' use-cache on
        zstyle ':completion:*' cache-path "$HOME/.zsh/cache"

        # Perform the original completion that triggered this
        zle expand-or-complete
      }

      zle -N _lazy_compinit
      bindkey '^I' _lazy_compinit

      # Lazy loading for Node / NVM
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

      # Foot OSC-133 integration for copying last command output
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
    '';
  };
}