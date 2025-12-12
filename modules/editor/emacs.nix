{pkgs, ...}: {
  # Symlink Emacs configuration directory
  home.file.".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
  };

  programs.emacs = {
    enable = true;

    # Use emacs-pgtk for Wayland native support
    package = pkgs.emacs-pgtk;

    # Minimal package set for initial configuration
    extraPackages = epkgs: let
      claude-code = epkgs.trivialBuild {
        pname = "claude-code";
        version = "main";
        src = pkgs.fetchFromGitHub {
          owner = "stevemolitor";
          repo = "claude-code.el";
          rev = "main";
          sha256 = "0z77nxazkw08pmqam2z27a56s9nyp72a1vvc0ba3vgcwfkjx0v81";
        };
        packageRequires = with epkgs; [transient inheritenv];
      };
    in
      with epkgs; [
        # Performance
        gcmh

        # Version Control
        magit

        # Undo System
        undo-fu
        undo-fu-session

        # Evil mode (Vim emulation)
        evil
        evil-collection
        evil-surround

        # Discovery
        which-key

        # Keybindings
        general

        # Completion
        vertico
        orderless
        consult
        marginalia
        embark
        embark-consult

        # Development
        eglot
        treesit-auto

        # Terminal
        vterm

        # Org Mode
        org-roam

        # Theme & UI
        doom-themes # Doom Emacs themes including doom-nord
        doom-modeline
        nerd-icons

        # use-package is built-in on Emacs 29+, but we ensure it's available
        use-package

        # Claude Code Integration
        claude-code
        inheritenv
        eat
      ];
  };

  # Enable native compilation warnings suppression during build
  home.sessionVariables = {
    EMACS_NATIVE_COMP_ASYNC_REPORT_WARNINGS_ERRORS = "nil";
  };
}
