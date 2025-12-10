{whichkey, ...}: {
  home.packages = [
    whichkey.packages.x86_64-linux.wlr-which-key
  ];

  xdg.configFile."wlr-which-key/config.yaml".text = ''
    # Comprehensive System Menu Configuration
    font: JetBrainsMono Nerd Font 11
    background: "#282828d0"
    color: "#ffffff"
    border: "#ffffff"
    separator: " → "
    border_width: 2
    corner_r: 8
    padding: 12
    rows_per_column: 15
    column_padding: 20

    # Positioning
    anchor: center
    namespace: "wlr_which_key"
    inhibit_compositor_keyboard_shortcuts: true
    auto_kbd_layout: true

    menu:

      # ai
      - key: "a"
        desc: ai
        submenu:
          - key: "p"
            desc: prompt library
            cmd: foot -a filepicker -e copy-prompt
          - key: "n"
            desc: nix
            cmd: kitty -d /home/joebutler/nix-config gemini

      # clipboard
      - key: "c"
        desc: clipboard
        cmd: foot -a clipse -D ~ sh -lc clipse

      # files
      - key: "f"
        desc: files
        submenu:
          - key: "f"
            desc: find files
            cmd: foot -a filepicker -e fzf-file-launcher
          - key: "p"
            desc: path
            cmd: foot -a dirfinder -e directory-finder
          - key: "r"
            desc: recent files
            cmd: foot -a filepicker -e recent-files-launcher

      # workflows
      - key: "p"
        desc: projects
        submenu:
          - key: "h"
            desc: health
            cmd: ~/documents/projects/sys-arc/project-hooks/health.sh
          - key: "e"
            desc: emacs
            cmd: ~/documents/projects/sys-arc/project-hooks/learn-emacs.sh
          - key: "n"
            desc: nix
            cmd: ~/documents/projects/sys-arc/project-hooks/learn-nix.sh
          - key: "m"
            desc: misc
            cmd: ~/documents/projects/sys-arc/project-hooks/misc.sh
          - key: "a"
            desc: sys arc
            cmd: ~/documents/projects/sys-arc/project-hooks/sys-arc.sh

      # workflows
      - key: "w"
        desc: workflows
        submenu:
          - key: "s"
            desc: study mode
            cmd: study-focus toggle
  '';
}
