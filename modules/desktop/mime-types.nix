_: {
  # XDG MIME associations (system-level)
  environment.etc."xdg/mimeapps.list".text = ''
    [Default Applications]
    text/html=brave-gatekeeper.desktop
    x-scheme-handler/http=brave-gatekeeper.desktop
    x-scheme-handler/https=brave-gatekeeper.desktop
    x-scheme-handler/about=brave-gatekeeper.desktop
    x-scheme-handler/unknown=brave-gatekeeper.desktop

    application/pdf=org.pwmt.zathura.desktop
    application/x-pdf=org.pwmt.zathura.desktop

    image/jpeg=viewnior.desktop
    image/png=viewnior.desktop
    image/gif=viewnior.desktop
    image/webp=viewnior.desktop
    image/svg+xml=viewnior.desktop

    text/plain=emacsclient.desktop
    text/markdown=emacsclient.desktop
    text/x-tex=emacsclient.desktop
    text/x-nix=emacsclient.desktop
    text/x-shellscript=emacsclient.desktop
    application/x-shellscript=emacsclient.desktop

    inode/directory=thunar.desktop
  '';
}
