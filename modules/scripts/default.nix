{
  pkgs,
  user,
  ...
}: {
  home.packages = [
    (import ./file-launcher.nix {inherit pkgs;})
    (import ./recent-files-launcher.nix {inherit pkgs;})
    (import ./copy-prompt.nix {inherit pkgs user;})
    (import ./directory-finder.nix {inherit pkgs;})
    (import ./study-focus.nix {inherit pkgs;})
    (import ./rofi-daily-capture.nix {inherit pkgs;})
    (import ./maintenance/weekly-review.nix {inherit pkgs;})
  ];
}
