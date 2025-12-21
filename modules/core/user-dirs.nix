{user, ...}: {
  # XDG user directories (system-level)
  environment.etc."xdg/user-dirs.defaults".text = ''
    DESKTOP=/home/${user}/desktop
    DOCUMENTS=/home/${user}/documents
    DOWNLOAD=/home/${user}/downloads
    PICTURES=/home/${user}/pictures

    # Point unwanted directories to $HOME to prevent their creation
    MUSIC=/home/${user}
    VIDEOS=/home/${user}
    PUBLICSHARE=/home/${user}
    TEMPLATES=/home/${user}
  '';
}
