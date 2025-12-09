_: {
  wayland.windowManager.hyprland.extraConfig = ''
      # window rules
      windowrulev2 = suppressevent maximize, class:.*
      windowrulev2 = nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0

      windowrulev2 = float, class:filepicker
      windowrulev2 = center, class:filepicker
      windowrulev2 = size 70% 40%, class:filepicker
      windowrulev2 = stayfocused, class:filepicker
      windowrulev2 = float, class:dirfinder
      windowrulev2 = center, class:dirfinder
      windowrulev2 = size 600 120, class:dirfinder
      windowrulev2 = opacity 0.9, class:dirfinder
      windowrulev2 = float, class:clipse
      windowrulev2 = center, class:clipse
      windowrulev2 = size 70% 40%, class:clipse
      windowrulev2 = stayfocused, class:clipse
      windowrulev2 = float, persistentsize, class:blueman-manager

      # brave-gatekeeper
      windowrulev2 = float, class:^(brave-gatekeeper)$
      windowrulev2 = center, class:^(brave-gatekeeper)$
    windowrulev2 = size 300 300, class:^(brave-gatekeeper)$
    windowrulev2 = stayfocused, class:^(brave-gatekeeper)$
      windowrulev2 = bordersize 1, class:^(brave-gatekeeper)$
      windowrulev2 = bordercolor rgb(ffffff), class:^(brave-gatekeeper)$

      # terminal border
      windowrulev2 = bordersize 1, class:^kitty$
      windowrulev2 = bordercolor rgb(ffffff), class:^kitty$
  '';
}
