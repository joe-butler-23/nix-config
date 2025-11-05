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

  # files
  - key: "f"
    desc: files
    submenu:
      - key: "f"
        desc: find files
        cmd: footclient -a filepicker -e /home/joebutler/bin/fzf_file_launcher.sh
      - key: "p"
        desc: path
        cmd: footclient -a dirfinder -D ~ sh -lc /home/joebutler/bin/directory-finder.sh
      - key: "r"
        desc: recent files
        cmd: footclient -a filepicker -e /home/joebutler/bin/recent_files_launcher.sh

  # clipboard
  - key: "c"
    desc: clipboard
    cmd: footclient -a clipse -D ~ sh -lc clipse

  # workflows
  - key: "w"
    desc: workflows
    submenu:
      - key: "s"
        desc: study mode
        cmd: /home/joebutler/bin/study-focus/study-focus toggle
  '';
}
