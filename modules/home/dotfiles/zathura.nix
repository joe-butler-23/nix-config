_: {
  programs.zathura = {
    enable = true;
    options = {
      selection-clipboard = "clipboard";
      guioptions = "shv";
      synctex = true;
      synctex-editor-command = "nvr --remote-silent +%{line} %{input}";
    };
  };
}
