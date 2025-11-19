{
  pkgs,
  anki-forge,
  ...
}: let
  # Smart Anki Launcher: Warns if already running
  ankiSmart = pkgs.writeShellScriptBin "anki-smart" ''
    if pgrep -x "anki" > /dev/null; then
      ${pkgs.libnotify}/bin/notify-send -u normal "Anki is already running" "Check your scratchpad (Special Workspace)"
    else
      anki "$@"
    fi
  '';

  # Workflow Launcher: Ensures Anki in scratchpad + Forge
  ankiForgeLauncher = pkgs.writeShellScriptBin "anki-forge-launcher" ''
    # Check if Anki is running
    if ! pgrep -x "anki" > /dev/null; then
      # Launch Anki directly into the 'magic' special workspace (scratchpad)
      echo "Launching Anki in scratchpad..."
      hyprctl dispatch exec "[workspace special:magic] anki"
    else
      echo "Anki is already running."
      ${pkgs.libnotify}/bin/notify-send -u low "Anki already active" "Launching Anki Card Forge..."
    fi

    # Launch the main app
    anki-card-forge
  '';
in {
  home.packages = [
    anki-forge.packages.x86_64-linux.default
    ankiSmart
    ankiForgeLauncher
    pkgs.libnotify
  ];
}
