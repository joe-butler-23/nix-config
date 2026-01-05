{
  pkgs,
  user,
  ...
}: {
  # Rclone mount service for Google Drive
  # Configuration managed by sops-nix
  systemd.user.services.rclone-mount = {
    description = "Google Drive Mount (Rclone)";
    after = ["network-online.target" "sops-nix.service"];
    wants = ["network-online.target" "sops-nix.service"];
    wantedBy = ["default.target"];

    serviceConfig = {
      Type = "notify";
      # Ensure mount point and cache directory exist
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /home/${user}/GoogleDrive /home/${user}/.cache/rclone";
      # Mount configured 'gdrive' remote to ~/GoogleDrive
      # Config file is provided by sops-nix at the default location
      ExecStart = "${pkgs.rclone}/bin/rclone mount gdrive: /home/${user}/GoogleDrive --vfs-cache-mode full --no-modtime";
      ExecStop = "/run/wrappers/bin/fusermount -u -z /home/${user}/GoogleDrive";

      # --- Security Hardening ---
      ProtectSystem = "strict";
      ProtectHome = "read-only";
      # Allow writing only to mount point and cache
      ReadWritePaths = [
        "/home/${user}/GoogleDrive"
        "/home/${user}/.cache/rclone"
      ];
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectControlGroups = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      # --------------------------

      Restart = "on-failure";
      RestartSec = "10s";
      Environment = ["PATH=/run/wrappers/bin:${pkgs.coreutils}/bin"];
    };
  };

  # SOPS secret for rclone config
  sops.secrets.rclone_config = {
    sopsFile = ../../secrets/secrets.yaml;
    mode = "0600";
    owner = user;
    path = "/home/${user}/.config/rclone/rclone.conf";
  };
}
