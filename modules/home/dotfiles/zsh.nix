{ config, pkgs, lib, user, ... }: {

  # 1. Symlink P10k Config (Must be outside programs.zsh)
  # This takes the file from your repo and puts it at ~/.p10k.zsh
  home.file.".p10k.zsh".source = ./p10k.zsh;

  programs.zsh = {
    enable = true;

    # 2. Optimize Startup
    # We enable completion installation but DISABLE the slow default initialization.
    # Your _lazy_compinit script below handles it much faster.
    enableCompletion = true;

    # 3. Plugins (Powerlevel10k)
    plugins = [
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
    ];

    # 4. History
    history = {
      size = 10000;
      save = 10000;
      path = "${config.xdg.dataHome}/zsh/history";
      ignoreAllDups = true;
      ignoreSpace = true;
      share = false;
      extended = false;
    };

    # 5. Built-ins
    syntaxHighlighting.enable = true;
    autosuggestion = {
      enable = true;
      strategy = ["completion" "history"];
    };
    historySubstringSearch.enable = true;

    # 6. Aliases
    shellAliases = {
      hs = "home-manager switch --flake \"$HOME/nix-config#${user}\"";
      ns = "sudo nixos-rebuild switch --flake \"$HOME/nix-config\"";
      hsdry = "home-manager build --flake \"$HOME/nix-config#${user}\"";
      nsdry = "sudo nixos-rebuild dry-build --flake \"$HOME/nix-config\"";
      hn = "home-manager news";
      hstatus = "home-manager generations";
    };

    # 7. Initialization Scripts
    initExtra = lib.mkMerge [

      # --- PART A: INSTANT PROMPT (Must run first) ---
      (lib.mkBefore ''
        # Enable Powerlevel10k instant prompt.
        # Should stay close to the top of ~/.zshrc.
        if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
          source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
        fi
      '')

      # --- PART B: MAIN CONFIGURATION ---
      (lib.mkOrder 1200 ''
        # Load the p10k config (via the symlink we created above)
        source ~/.p10k.zsh

        # Core options
        setopt correct extendedglob nocaseglob rcexpandparam nocheckjobs numericglobsort nobeep appendhistory histignorealldups autocd inc_append_history histignorespace interactivecomments

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

        # Yazi chooser
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

        # --- LAZY LOADING OPTIMIZATIONS ---

        # Ensure cache directory exists
        [[ -d "$HOME/.zsh/cache" ]] || mkdir -p "$HOME/.zsh/cache"

        # Lazy Completion (Triggers on first TAB press)
        _lazy_compinit() {
          unfunction _lazy_compinit 2>/dev/null
          autoload -Uz compinit
          local zcompdump="$HOME/.zsh/cache/zcompdump-$ZSH_VERSION"
          
          # Compile cache in background if needed
          if [[ -s "$zcompdump" && ( ! -s "$zcompdump.zwc" || "$zcompdump" -nt "$zcompdump.zwc" ) ]]; then
            zcompile "$zcompdump" &!
          fi
          
          compinit -C -d "$zcompdump"
          
          # Styles
          zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
          zstyle ':completion:*' list-colors "''${(s.:.)LS_COLORS}"
          zstyle ':completion:*' rehash true
          zstyle ':completion:*' menu select
          zstyle ':completion:*' accept-exact '*(N)'
          zstyle ':completion:*' use-cache on
          zstyle ':completion:*' cache-path "$HOME/.zsh/cache"
          
          zle expand-or-complete
        }
        zle -N _lazy_compinit
        bindkey '^I' _lazy_compinit

        # Lazy NVM
        if [ -d "$HOME/.nvm" ]; then
          _lazy_nvm_bootstrap() {
            unfunction _lazy_nvm_bootstrap 2>/dev/null
            unset -f node npm npx corepack yarn pnpm 2>/dev/null
            export NVM_DIR="$HOME/.nvm"
            [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
          }
          node()      { _lazy_nvm_bootstrap; command node "$@"; }
          npm()       { _lazy_nvm_bootstrap; command npm "$@"; }
          npx()       { _lazy_nvm_bootstrap; command npx "$@"; }
          corepack()  { _lazy_nvm_bootstrap; command corepack "$@"; }
          yarn()      { _lazy_nvm_bootstrap; command yarn "$@"; }
          pnpm()      { _lazy_nvm_bootstrap; command pnpm "$@"; }
        fi
      '')
    ];
  };
}