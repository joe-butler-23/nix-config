_: {
  programs.starship = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      command_timeout = 2000;

      directory.read_only = " 󰌾";
      git_commit.tag_symbol = "  ";
      memory_usage.symbol = "󰍛 ";
      meson.symbol = "󰔷 ";
      nim.symbol = "󰆥 ";
      nodejs.disabled = true;
      rlang.symbol = "󰟔 ";
      rust.symbol = "󱘗 ";

      os.symbols = {
        Garuda = "󰛓 ";
        HardenedBSD = "󰞌 ";
        Illumos = "󰈸 ";
        OpenBSD = "󰈺 ";
        OracleLinux = "󰌷 ";
        Redox = "󰀘 ";
        Solus = "󰠳 ";
        Windows = "󰍲 ";
      };
    };
  };
}
