_: {
  # Mousepad (XFCE Text Editor) configuration
  xdg.configFile = {
    # Main settings
    "Mousepad/settings.conf".text = ''
      [org/xfce/mousepad/state/application]
      session=[]

      [org/xfce/mousepad/state/window]
      maximized=false
      fullscreen=false

      [org/xfce/mousepad/preferences/view]
      tab-width=2
      show-line-numbers=true
      highlight-current-line=true
      show-right-margin=true
      right-margin-position=80
      show-whitespace=false
      word-wrap=true

      [org/xfce/mousepad/preferences/editor]
      font-name="JetBrains Mono 11"
      auto-indent=true
      smart-home-end=true
      smart-backspace=true

      [org/xfce/mousepad/state/search]
      search-history=[]
    '';

    # Keyboard accelerators
    "Mousepad/accels.scm".text = ''
      ; mousepad GtkAccelMap rc-file         -*- scheme -*-
      ; this file is an automated accelerator map dump
      ;
      ; (gtk_accel_path "<Mousepad>/file-menu/new" "<Primary>n")
      ; (gtk_accel_path "<Mousepad>/file-menu/open" "<Primary>o")
      ; (gtk_accel_path "<Mousepad>/file-menu/save" "<Primary>s")
      ; (gtk_accel_path "<Mousepad>/file-menu/save-as" "<Primary><Shift>s")
      ; (gtk_accel_path "<Mousepad>/edit-menu/undo" "<Primary>z")
      ; (gtk_accel_path "<Mousepad>/edit-menu/redo" "<Primary><Shift>z")
      ; (gtk_accel_path "<Mousepad>/edit-menu/cut" "<Primary>x")
      ; (gtk_accel_path "<Mousepad>/edit-menu/copy" "<Primary>c")
      ; (gtk_accel_path "<Mousepad>/edit-menu/paste" "<Primary>v")
      ; (gtk_accel_path "<Mousepad>/edit-menu/select-all" "<Primary>a")
      ; (gtk_accel_path "<Mousepad>/search-menu/find" "<Primary>f")
      ; (gtk_accel_path "<Mousepad>/search-menu/find-next" "<Primary>g")
      ; (gtk_accel_path "<Mousepad>/search-menu/replace" "<Primary>h")
    '';
  };
}
