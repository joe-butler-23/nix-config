{
  pkgs,
  user,
  ...
}: {
  # Rclone mount service for Google Drive
  # Requires 'gdrive' remote to be configured in rclone config
  # Run: rclone config -> n (new remote) -> name: gdrive -> Storage: drive
  systemd.user.services.rclone-mount = {
    description = "Google Drive Mount (Rclone)";
    after = ["network-online.target"];
    wants = ["network-online.target"];
    wantedBy = ["default.target"];

    serviceConfig = {
      Type = "notify";
      # Ensure mount point exists
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /home/${user}/GoogleDrive";
      # Mount configured 'gdrive' remote to ~/GoogleDrive
      # --vfs-cache-mode full: Recommended for best compatibility (allows writes/random access)
      ExecStart = "${pkgs.rclone}/bin/rclone mount gdrive: /home/${user}/GoogleDrive --vfs-cache-mode full --no-modtime";
      ExecStop = "/run/wrappers/bin/fusermount -u -z /home/${user}/GoogleDrive";
      Restart = "on-failure";
      RestartSec = "10s";
      Environment = ["PATH=/run/wrappers/bin:${pkgs.coreutils}/bin"];
    };
  };
}
