# NixOS Installation Guide

## Initial Setup

### WiFi Configuration

To configure the WiFi, first start wpa_supplicant:

```
sudo systemctl start wpa_supplicant

wpa_cli
```

Then connect to a network

```
add_network
0
set_network 0 ssid "myhomenetwork"
OK
set_network 0 psk "mypassword"
OK
enable_network 0
OK
```

To ssh in, establish the IP address with:

```
# Establish device
ip a

# Identify ip address 
ip addr show wlp3s0 # Where wlp3s0 is the device na,e

# Set password
sudo passwd nixos
```
From the other machine: `ssh nixos@192.168.0.xxx`.

## Disk Encryption and Partitioning

### Identify Disk

Run `lsblk` to identify disk. Then run `sudo gdisk /dev/sdx` where sdx is the target drive. Typically it is nvme0n1.

#### Wipe and create a new GPT table

```bash
sudo gdisk /dev/nvme0n1
```

Inside gdisk:

- `o`   → create a new empty GPT
- `y`   → confirm
- `w`   → write table
- `y`   → confirm

---

#### Create 2 partitions

Partition 1 — EFI (1 GiB)

- `n`
- press `Enter` to accept default partition number
- press `Enter` to accept default first sector
- type `+1G` and press `Enter` for last sector
- type `EF00` for hex code (EFI System)

Partition 2 — rest of disk

- `n`
- press `Enter`
- press `Enter`
- press `Enter`
- type `8300` for hex code (Linux filesystem)

Write the table

- `w`
- `y`

---

#### Verify

```bash
lsblk -o NAME,SIZE,TYPE,MOUNTPOINTS
```

This should show should show two partitions as /dev/sdx1 and /dev/sdx2

### Enable Encryption

Enable encryption on the non-boot part of the drive with the cryptsetup:

Cryptsetup has the following options:

- `-v`: Verbose, increases output for debugging in case something goes wrong.
- `-y`: Ask for the password interactively, twice, and ensure their match before proceeding.
- `-c`: Specifies the cypher, in this case aes-xts-plain64 is also the default for the LUKS2 format.
- `-s`: Specifies the key size used by the cypher.
- `-h`: Specifies the hashing algorith used, sha256 by default.
- `-i`: Milliseconds to spend processing the passphrase, 2000 by default.
- `--use-random`: Specifies the more secure RNG source.
- `--label`: Adds a label to the partition so we can reference it easily in configs.
- `luksFormat`: Operation mode that encrypts a partition and sets a passphrase.
- `--type`: Specify the LUKS type to use.
- `/dev/sdx2`: The partition you wish to encrypt (typically something like /dev/nvme0n1p2)

So typical command is most likely to be:

```bash
sudo cryptsetup -v -y \
    -c aes-xts-plain64 -s 512 -h sha512 -i 2000 --use-random \
    --label=NIXOS_LUKS luksFormat --type luks2 /dev/nvme0n1p2
```

It should warn about overwriting data, and then let you set a password. 

### Verify and Open Encrypted Container

Inspect the header to check it went well: `sudo cryptsetup luksDump /dev/nvme0n1p2`.

Open the container: `sudo cryptsetup open --type luks /dev/nvme0n1p2 cryptroot`.

Inspect mapped device: `ls /dev/mapper/cryptroot`.

### Create Logical Volumes

Create new logical volume:

```bash
sudo pvcreate /dev/mapper/cryptroot
sudo vgcreate lvmroot /dev/mapper/cryptroot
sudo lvcreate -l 100%FREE lvmroot -n root
```

Verify result with: `lsblk -f`.

## Btrfs Setup

### Format and Create Subvolumes

Now format btrfs and create subvolumes:

```bash
sudo mkfs.btrfs -L NIXOS_ROOT /dev/mapper/lvmroot-root

sudo mount /dev/mapper/lvmroot-root /mnt

sudo btrfs subvolume create /mnt/root
sudo btrfs subvolume create /mnt/home
sudo btrfs subvolume create /mnt/nix
sudo btrfs subvolume create /mnt/persist
sudo btrfs subvolume create /mnt/log
```

### Create Blank Root Snapshot

For the impermanence module (doesnt' have to be enabled now), a blank snapshot of the root is needed:

```bash
sudo btrfs subvolume snapshot -r /mnt/root /mnt/root-blank
sudo umount /mnt
```

### Mount Subvolumes

Now remount everything in the right place:

```bash
sudo mount -o subvol=root,compress=zstd,noatime /dev/mapper/lvmroot-root /mnt

sudo mkdir /mnt/boot
sudo mount /dev/nvme0n1p1 /mnt/boot
sudo mkdir /mnt/home
sudo mount -o subvol=home,compress=zstd,noatime /dev/mapper/lvmroot-root /mnt/home
sudo mkdir /mnt/nix
sudo mount -o subvol=nix,compress=zstd,noatime /dev/mapper/lvmroot-root /mnt/nix
sudo mkdir /mnt/persist
sudo mount -o subvol=persist,compress=zstd,noatime /dev/mapper/lvmroot-root /mnt/persist
sudo mkdir -p /mnt/var/log
sudo mount -o subvol=log,compress=zstd,noatime /dev/mapper/lvmroot-root /mnt/var/log
```

### Verify Mounts

Review mounts before continuing:

```bash
lsblk -f
mount | grep /mnt
```

Or for simpler view run `sudo lsblk -o name,type,mountpoints /dev/nvme0n1` for a tree diagram that should look like:

```
NAME               TYPE  MOUNTPOINTS
nvme0n1            disk  
├─nvme0n1p1        part  /mnt/boot
└─nvme0n1p2        part  
  └─cryptroot      crypt 
    └─lvmroot-root lvm   /mnt/var/log
                         /mnt/persist
                         /mnt/nix
                         /mnt/home
                         /mnt
```

## Configuration Setup

### Generate Base Config

```bash
sudo nixos-generate-config --root /mnt
```

This generates the initial config, which must be amended to account for the encryption and partition set-up. So identify the config files and the required disk info:

```bash
ls -l /mnt/etc/nixos/

# LUKS container (nvme0n1p2) — use LABEL or UUID
ls -l /dev/disk/by-label/ | grep NIXOS_LUKS || true
ls -l /dev/disk/by-uuid/  | grep 1937b868-0ba6-45ad-9252-6c45c61ae86c || true

# Btrfs root LV
ls -l /dev/disk/by-uuid/  | grep db52d953-3a83-4c18-b9ab-af6ced62bb6f || true
```

### Amend Hardware Configuration

## this needs amending since the guidance refers to an approach for ext4 rather than btrfs. so will likely create issues. 

First amend the hardware-configuration, with `sudo nano /mnt/etc/nixos/hardware-configuration.nix`. Follow the instructions here https://jadarma.github.io/blog/posts/2024/08/installing-nixos-with-flakes-and-lvm-on-luks/.

In particular, add `"compress=zstd" "noatime"` to each of the btrfs options, i.e. `'options = [ "subvol=persist"  "compress=zstd" "noatime" ];'`. Also set fmask=0077 and dmask=0077 for boot, and change the partition names to `/dev/disk/by-label`. And make sure the top looks like this to enable cryptd:

```nix
boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
boot.initrd.kernelModules = [ "dm-snapshot" "cryptd" ]; # <--
boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-label/NIXOS_LUKS"; # <--
boot.kernelModules = [ "kvm-intel" ];
boot.extraModulePackages = [ ];
```

In nano its ctrl + w to exit, press y to save. 

There are then different options depending on wheterh the intention is to use an existing config or start fresh. To use the config in this repo first: 

## Install with existing nix config

```
# Clone flake beside /etc/nixos
sudo mkdir -p /mnt/root
sudo git clone https://github.com/joe-butler-23/nix-config.git /mnt/root/nix-config

# Copy the already-generated + amended hardware config into flake repo
sudo cp -v /mnt/etc/nixos/hardware-configuration.nix /mnt/root/nix-config/hardware-configuration.nix

# Verify the flake outputs to confirm the host attr (expects `nixos`)
sudo nix --extra-experimental-features 'nix-command flakes' \
  flake show /mnt/root/nix-config
```

Once installation completes, set a user password:

```
sudo nixos-enter --root /mnt -- sh -c 'passwd joebutler'
```

Then unmount and reboot:

```
sudo umount -R /mnt
sudo cryptsetup close cryptroot
sudo reboot
```

I have had problems in the past with user not being set up properly, in which case log in to shell as root and do the following:

```
mkdir -p /home/joebutler
chown -R joebutler:users /home/joebutler
passwd joebutler
```

And then need to do some configuring for the full nix config [for future, I think I can skip generating any of the configs, and just go straight to a nix-install with my github configs to miss out the steps here, since the hardware config in my repo uses labels rather than UUIDs, but make `sure sudo dosfslabel /dev/nvme0n1p1 NIXOS_BOOT` to ensure the hardware config file works]. Connect to internet, then:

```
# clone your repo
git clone https://github.com/joe-butler-23/nix-config.git /home/joebutler/nix-config

# ensure ownership and restrictive perms
sudo chown -R joebutler:users /home/joebutler/nix-config
chmod -R go-rwx /home/joebutler/nix-config

# replace /etc/nixos with symlink to your repo
sudo rm -rf /etc/nixos
sudo ln -s /home/joebutler/nix-config /etc/nixos

# rebuild
cd /home/joebutler/nix-config
sudo nixos-rebuild switch --flake .

```


## Alternatively, install with fresh nix setup

### Edit Configuration.nix

Then edit configuration.nix with `sudo nano /mnt/etc/nixos/configuration.nix`. Follow the instructions here https://jadarma.github.io/blog/posts/2024/08/installing-nixos-with-flakes-and-lvm-on-luks/ for the configuration file.

## Flake Setup

### Generate Flake

Create `/mnt/etc/nixos/flake.nix` with the following contents:

```nix
{
  description = "My NixOS Config";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };
  outputs = inputs@{ self, nixpkgs, ...}: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./configuration.nix ];
    };
  };
}
```

Be sure to check the system number (25.05) matches whatever appeared in the config file.

This can be done in a single command:

```bash
sudo sh -c 'cat > /mnt/etc/nixos/flake.nix <<EOF
{
  description = "My NixOS Config";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };
  outputs = inputs@{ self, nixpkgs, ...}: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./configuration.nix ];
    };
  };
}
EOF'
```

Double check with `cat /mnt/etc/nixos/flake.nix`.

## Installation

### Run Installer

Now run the installer:

```bash
cd /mnt/etc/nixos
sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes flake update
cd
```

When lockfile created:

```bash
sudo nixos-install --root /mnt --no-root-passwd --flake /mnt/etc/nixos#nixos
```

### Set Password

Then set password:

```bash
sudo nixos-enter --root /mnt -c 'passwd'
```

And then unmount and reboot:

```bash
sudo umount -R /mnt
sudo swapoff -L NIXOS_SWAP
sudo vgchange -a n lvmroot
sudo cryptsetup close /dev/mapper/cryptrootew
sudo reboot
```

## Post-reboot

### Boot Permissions

Double check boot permissions:

```bash
sudo chmod 700 /boot
sudo chmod 600 /boot/loader/random-seed
```

### Git Repository Setup 

Establish NixOS as git repo:

```bash
git config --global user.name "Your Name"
git config --global user.email "your@email.com"

mkdir -p ~/repo/nixfiles
cd ~/repo/nixfiles

cp -r /etc/nixos/* .

git init
git add .
git commit -m "Initial commit"
```

### GitHub Integration

Create GitHub repo called nixos-config and create SSH key for new machine:

```bash
# 1) Create a new Ed25519 key (use the same email as your GitHub account)
ssh-keygen -t ed25519 -C "your-email@example.com"

# Press Enter to accept the default path (~/.ssh/id_ed25519)
# Optionally set a passphrase

# 2) Start the agent and add the key
eval "$(ssh-agent -s)"
install -m 700 -d ~/.ssh
ssh-add ~/.ssh/id_ed25519

# 3) Show your public key (copy everything on this line)
cat ~/.ssh/id_ed25519.pub
```

Copy full output and save to GitHub online. Then:

```bash
git remote add origin git@github.com:joe-butler-23/nixos-config.git
git branch -M main
git push -u origin main
```

## Resources

- https://guekka.github.io/nixos-server-1/
- https://jadarma.github.io/blog/posts/2024/08/installing-nixos-with-flakes-and-lvm-on-luks/
