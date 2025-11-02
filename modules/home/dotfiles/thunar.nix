_: {
  # Thunar (XFCE File Manager) configuration
  xdg.configFile = {
    # Custom actions configuration
    "Thunar/uca.xml".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <actions>
      <action>
      	<icon>utilities-terminal</icon>
      	<name>Open Terminal Here</name>
      	<submenu></submenu>
      	<unique-id>1761744613086665-1</unique-id>
      	<command>exo-open --working-directory %f --launch TerminalEmulator</command>
      	<description>Open terminal in the current directory</description>
      	<range></range>
      	<patterns>*</patterns>
      	<startup-notify/>
      	<directories/>
      </action>
      </actions>
    '';

    # Keyboard accelerators
    "Thunar/accels.scm".text = ''
      ; thunar GtkAccelMap rc-file         -*- scheme -*-
      ; this file is an automated accelerator map dump
      ;
      ; (gtk_accel_path "<Actions>/ThunarActions/uca-action-1761744613086665-1" "")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/create-folder" "<Primary><Shift>n")
      ; (gtk_accel_path "<Actions>/ThunarLauncher/delete" "<Primary>Delete")
      ; (gtk_accel_path "<Actions>/ThunarLauncher/delete" "Delete")
      ; (gtk_accel_path "<Actions>/ThunarLauncher/open" "<Primary>o")
      ; (gtk_accel_path "<Actions>/ThunarLauncher/open-with-other" "<Primary><Shift>o")
      ; (gtk_accel_path "<Actions>/ThunarLauncher/properties" "<Alt>Return")
      ; (gtk_accel_path "<Actions>/ThunarShortcutsPane/sendto-shortcuts" "<Primary>b")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/back" "<Alt>Left")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/back" "BackSpace")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/create-document" "")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/forward" "<Alt>Right")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/properties" "<Primary>i")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/select-all-files" "<Primary>a")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/select-by-pattern" "<Primary>s")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/sort-by-dtime" "")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/sort-by-mtime" "")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/sort-by-name" "")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/sort-by-size" "")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/sort-by-type" "")
    '';
  };
}
