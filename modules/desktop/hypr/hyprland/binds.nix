_: {
  wayland.windowManager.hyprland.extraConfig = ''
    # mod
    $mainMod = SUPER

    # terminal
    $terminal = kitty --single-instance

    # menu
    $menu = rofi -show drun -matching regex -no-tokenize -drun-match-fields 'name' -drun-display-format '{name}' -display-drun "" -theme-str 'textbox-prompt-colon { enabled: false; }' -run-command "app2unit -- {cmd}"
    $browser = brave

    # file manager
    $fileManager = app2unit $terminal --class yazi --directory ~ -e sh -lc 'TMPFILE=$(mktemp); yazi --chooser-file="$TMPFILE"; if [ -s "$TMPFILE" ]; then xdg-open "$(cat "$TMPFILE")"; fi; rm -f "$TMPFILE"'

    # apps
    bind = $mainMod, Return, exec, $terminal
    bind = $mainMod, C, killactive,
    bind = $mainMod, E, exec, $fileManager
    bind = $mainMod, V, togglefloating,
    bindr = $mainMod, SUPER_L, exec, $menu
    bind = $mainMod, P, pseudo,
    bind = $mainMod, U, togglesplit,
    bind = $mainMod, B, exec, $browser
    bind = $mainMod, Y, exec, app2unit hyprshot -m region --clipboard-only
    bind = $mainMod, SPACE, exec, wlr-which-key
    bind = $mainMod, a, exec, anki-forge-launcher
    bind = $mainMod, R, exec, emacsclient -c -F "((name . \"refile.org\"))" -a "" ~/refile.org

    # fzf launcher
    bind = $mainMod, F, exec, app2unit $terminal --class filepicker -e fzf-file-launcher

    bind = $mainMod, h, movefocus, l
    bind = $mainMod, l, movefocus, r
    bind = $mainMod, k, movefocus, u
    bind = $mainMod, j, movefocus, d

    bind = $mainMod, 1, workspace, 1
    bind = $mainMod, 2, workspace, 2
    bind = $mainMod, 3, workspace, 3
    bind = $mainMod, 4, workspace, 4
    bind = $mainMod, 5, workspace, 5
    bind = $mainMod, 6, workspace, 6
    bind = $mainMod, 7, workspace, 7
    bind = $mainMod, 8, workspace, 8
    bind = $mainMod, 9, workspace, 9
    bind = $mainMod, 0, workspace, 10
    bind = $mainMod SHIFT, h, workspace, e-1
    bind = $mainMod SHIFT, l, workspace, e+1

    bind = $mainMod SHIFT, 1, movetoworkspace, 1
    bind = $mainMod SHIFT, 2, movetoworkspace, 2
    bind = $mainMod SHIFT, 3, movetoworkspace, 3
    bind = $mainMod SHIFT, 4, movetoworkspace, 4
    bind = $mainMod SHIFT, 5, movetoworkspace, 5
    bind = $mainMod SHIFT, 6, movetoworkspace, 6
    bind = $mainMod SHIFT, 7, movetoworkspace, 7
    bind = $mainMod SHIFT, 8, movetoworkspace, 8
    bind = $mainMod SHIFT, 9, movetoworkspace, 9
    bind = $mainMod SHIFT, 0, movetoworkspace, 10

    bind = $mainMod, S, togglespecialworkspace, magic
    bind = $mainMod SHIFT, S, movetoworkspace, special:magic

    bind = $mainMod, mouse_down, workspace, e+1
    bind = $mainMod, mouse_up, workspace, e-1
    bind = , mouse:275, workspace, e-1
    bind = , mouse:276, workspace, e+1

    bindm = $mainMod, mouse:272, movewindow
    bindm = $mainMod, mouse:273, resizewindow

    bindel = ,XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+
    bindel = ,XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
    bindel = ,XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
    bindel = ,XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle
    bindel = ,XF86MonBrightnessUp, exec, brightnessctl s 10%+
    bindel = ,XF86MonBrightnessDown, exec, brightnessctl s 10%-
  '';
}
