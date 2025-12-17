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

       # gum-scripts
       windowrulev2 = float, class:^(gum-script)$
       windowrulev2 = center, class:^(gum-script)$
    		windowrulev2 = size 500 600, class:^(gum-script)$
     	windowrulev2 = stayfocused, class:^(gum-script)$
       windowrulev2 = bordersize 1, class:^(gum-script)$
       windowrulev2 = bordercolor rgb(ffffff), class:^(gum-script)$
    windowrulev2 = rounding 15, class:^(gum-script)$

       # refile - quick capture emacs
       windowrulev2 = float, title:^(refile\.org.*)$
       windowrulev2 = center, title:^(refile\.org.*)$
       windowrulev2 = size 400 400, title:^(refile\.org.*)$
       windowrulev2 = stayfocused, title:^(refile\.org.*)$
       windowrulev2 = bordersize 1, title:^(refile\.org.*)$
       windowrulev2 = bordercolor rgb(ffffff), title:^(refile\.org.*)$
       windowrulev2 = rounding 15, title:^(refile\.org.*)$

       # brave-wrapper
       windowrulev2 = float, class:^(brave-wrapper)$
       windowrulev2 = center, class:^(brave-wrapper)$
    		windowrulev2 = size 300 300, class:^(brave-wrapper)$
     	windowrulev2 = stayfocused, class:^(brave-wrapper)$
       windowrulev2 = bordersize 1, class:^(brave-wrapper)$
       windowrulev2 = bordercolor rgb(ffffff), class:^(brave-wrapper)$

        # nix rebuild
        windowrulev2 = float, class:^(nix_rebuild)$
        windowrulev2 = center, class:^(nix_rebuild)$
        windowrulev2 = size 300 300, class:^(nix_rebuild)$
        windowrulev2 = stayfocused, class:^(nix_rebuild)$
        windowrulev2 = bordersize 1, class:^(nix_rebuild)$
        windowrulev2 = bordercolor rgb(ffffff), class:^(nix_rebuild)$
        windowrulev2 = rounding 15, class:^(nix_rebuild)$

        # weekly review
        windowrulev2 = float, class:^(weekly_review)$
        windowrulev2 = center, class:^(weekly_review)$
        windowrulev2 = size 600 400, class:^(weekly_review)$
        windowrulev2 = stayfocused, class:^(weekly_review)$
        windowrulev2 = bordersize 1, class:^(weekly_review)$
        windowrulev2 = bordercolor rgb(ffffff), class:^(weekly_review)$
        windowrulev2 = rounding 15, class:^(weekly_review)$

        # terminal border
        windowrulev2 = bordersize 1, class:^kitty$
        windowrulev2 = bordercolor rgb(ffffff), class:^kitty$
  '';
}
