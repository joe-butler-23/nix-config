{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = ["dm-snapshot" "cryptd"];
  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-label/NIXOS_LUKS";
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_ROOT";
    fsType = "btrfs";
    options = ["subvol=root" "compress=zstd" "noatime"];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/NIXOS_BOOT";
    fsType = "vfat";
    options = ["fmask=0077" "dmask=0077"];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/NIXOS_ROOT";
    fsType = "btrfs";
    options = ["subvol=home" "compress=zstd" "noatime"];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/NIXOS_ROOT";
    fsType = "btrfs";
    options = ["subvol=nix" "compress=zstd" "noatime"];
  };

  fileSystems."/persist" = {
    device = "/dev/disk/by-label/NIXOS_ROOT";
    fsType = "btrfs";
    options = ["subvol=persist" "compress=zstd" "noatime"];
  };

  fileSystems."/var/log" = {
    device = "/dev/disk/by-label/NIXOS_ROOT";
    fsType = "btrfs";
    options = ["subvol=log" "compress=zstd" "noatime"];
  };

  swapDevices = [];

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.enableRedistributableFirmware = true;
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
