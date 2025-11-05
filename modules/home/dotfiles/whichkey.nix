{ pkgs, whichkey, ... }: {
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
      - key: "r"
        desc: RAG
        submenu:
          - key: "d"
            desc: dotfiles
            cmd: kitty --class filepicker -e gem_session dotfiles
          - key: "o"
            desc: obsidian vault
            cmd: kitty --class filepicker -e gem_session obsidian
          - key: "m"
            desc: manuals
            cmd: kitty --class filepicker -e gem_session manuals
      - key: "p"
        desc: prompt library
        cmd: kitty --class filepicker -e /home/joebutler/bin/prompt-launcher.sh
      - key: "x"
        desc: tester
        cmd: kitty --class filepicker -e /home/joebutler/bin/prompt-launcher.sh

  # files
  - key: "f"
    desc: files
    submenu:
      - key: "f"
        desc: find files
        cmd: kitty --class filepicker -e /home/joebutler/bin/fzf_file_launcher.sh
      - key: "p"
        desc: path
        cmd: footclient -a dirfinder -D ~ sh -lc /home/joebutler/bin/directory-finder.sh
      - key: "r"
        desc: recent files
        cmd: kitty --class filepicker -e /home/joebutler/bin/recent_files_launcher.sh

  # create
  - key: "c"
    desc: create
    submenu:
      - key: "p"
        desc: project
        cmd: kitty --class filepicker -e /home/joebutler/bin/project-manager add

  # projects
  - key: "p"
    desc: projects
    submenu:
      - key: "b"
        desc: budgeting
        submenu:
          - key: "c"
            desc: chatgpt
            cmd: "/usr/lib/brave-browser/brave --profile-directory=Default --app-id=cadlkienfkclaiaibeoongdcgmdikeeg"
          - key: "o"
            desc: opencode
            cmd: /home/joebutler/bin/ai-launcher "/home/joebutler/.local/share/Cryptomator/mnt/enc_vault/Obsidian_Vault/finances/budget"
      - key: "f"
        desc: fitness and health
        submenu:
          - key: "c"
            desc: chatgpt
            cmd: "/usr/lib/brave-browser/brave --profile-directory=Default --app-id=cadlkienfkclaiaibeoongdcgmdikeeg"
          - key: "o"
            desc: opencode
            cmd: /home/joebutler/bin/ai-launcher "/home/joebutler/.local/share/Cryptomator/mnt/enc_vault/Obsidian_Vault/health"
      - key: "h"
        desc: house
        submenu:
          - key: "c"
            desc: chatgpt
            cmd: "/usr/lib/brave-browser/brave --profile-directory=Default --app-id=cadlkienfkclaiaibeoongdcgmdikeeg"
          - key: "o"
            desc: opencode
            cmd: /home/joebutler/bin/ai-launcher "/home/joebutler/.local/share/Cryptomator/mnt/enc_vault/Obsidian_Vault/house"
      - key: "m"
        desc: meaningful money
        submenu:
          - key: "c"
            desc: chatgpt
            cmd: "/usr/lib/brave-browser/brave --profile-directory=Default --app-id=cadlkienfkclaiaibeoongdcgmdikeeg"
          - key: "o"
            desc: opencode
            cmd: /home/joebutler/bin/ai-launcher "/home/joebutler/.local/share/Cryptomator/mnt/enc_vault/Obsidian_Vault/finances/meaningful_money"
      - key: "v"
        desc: how to prove it
        submenu:
          - key: "c"
            desc: chatgpt
            cmd: "/usr/lib/brave-browser/brave --profile-directory=Default --app-id=cadlkienfkclaiaibeoongdcgmdikeeg"
          - key: "o"
            desc: opencode
            cmd: /home/joebutler/bin/ai-launcher "/home/joebutler/.local/share/Cryptomator/mnt/enc_vault/Obsidian_Vault/study/maths/how_to_prove_it"
      - key: "r"
        desc: reading
        submenu:
          - key: "c"
            desc: chatgpt
            cmd: "/usr/lib/brave-browser/brave --profile-directory=Default --app-id=cadlkienfkclaiaibeoongdcgmdikeeg"
          - key: "o"
            desc: opencode
            cmd: /home/joebutler/bin/ai-launcher "/home/joebutler/.local/share/Cryptomator/mnt/enc_vault/Obsidian_Vault/study/reading"
      - key: "s"
        desc: sys-arc
        submenu:
          - key: "c"
            desc: chatgpt
            cmd: "/usr/lib/brave-browser/brave --profile-directory=Default --app-id=cadlkienfkclaiaibeoongdcgmdikeeg"
          - key: "o"
            desc: opencode
            cmd: /home/joebutler/bin/ai-launcher "/home/joebutler/.local/share/Cryptomator/mnt/enc_vault/Obsidian_Vault/system"

  # quick actions
  - key: "q"
    desc: quick actions
    submenu:
      - key: "c"
        desc: clipboard management
        submenu:
          - key: "c"
            desc: clear clipboard
            cmd: clipse -clear
          - key: "h"
            desc: clipboard history
            cmd: footclient -a clipse -D ~ sh -lc clipse
          - key: "l"
            desc: start listener
            cmd: clipse -listen
          - key: "k"
            desc: kill listener
            cmd: clipse -kill

  # workflows
  - key: "w"
    desc: workflows
    submenu:
      - key: "s"
        desc: study mode
        cmd: /home/joebutler/bin/study-focus/study-focus toggle
  '';
}
