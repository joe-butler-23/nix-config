{ pkgs, ... }:

{
  # Enable Xfconf (required for Thunar settings)
programs.xfconf.enable = true;
programs.thunar.enable = true;


  # Thunar preferences via Xfconf
  xfconf.settings = {
    thunar = {
      # Show hidden files by default
      "last-show-hidden" = true;

      # Remember directory-specific view settings
      "misc-directory-specific-settings" = true;
    };
  };

  # Thunar config files
  xdg.configFile = {
    "Thunar/uca.xml".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <actions>
        <action>
          <icon>utilities-terminal</icon>
          <name>Open Terminal Here</name>
          <command>exo-open --working-directory %f --launch TerminalEmulator</command>
          <patterns>*</patterns>
          <directories/>
        </action>
      </actions>
    '';

    "Thunar/accels.scm".text = ''
      ; thunar GtkAccelMap rc-file -*- scheme -*-
      ; (gtk_accel_path "<Actions>/ThunarStandardView/create-folder" "<Primary><Shift>n")
      ; (gtk_accel_path "<Actions>/ThunarLauncher/delete" "<Primary>Delete")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/back" "<Alt>Left")
      ; (gtk_accel_path "<Actions>/ThunarStandardView/forward" "<Alt>Right")
    '';
  };
}
