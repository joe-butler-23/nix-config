{user, ...}: {
  # System-level environment variables
  environment.sessionVariables = {
    # Rust
    CARGO_HOME = "/home/${user}/.local/share/rust/cargo";
    RUSTUP_HOME = "/home/${user}/.local/share/rust/rustup";

    # Node / Bun
    BUN_INSTALL = "/home/${user}/.local/share/bun";
    npm_config_cache = "/home/${user}/.cache/npm";
    NPM_CONFIG_USERCONFIG = "/home/${user}/.config/npm/npmrc";

    # SOPS
    SOPS_AGE_KEY_FILE = "/home/${user}/nix-config/secrets/sops.agekey";

    # Qt platform (for Syncthing tray)
    QT_QPA_PLATFORM = "wayland";

    # Emacs
    EMACS_NATIVE_COMP_ASYNC_REPORT_WARNINGS_ERRORS = "nil";
  };
}
