_: {
  # ============================================================================
  # THUNAR USER CONFIGURATION
  # The system-level package installation and services (gvfs, tumbler) are in:
  # modules/core/sys-apps.nix
  # ============================================================================

  # Thunar preferences via Xfconf (Home Manager)
  xfconf.settings = {
    thunar = {
      "last-show-hidden" = true;
      "misc-directory-specific-settings" = true;
    };
  };

  # Thunar Custom Actions & Keybinds
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
