# Corne Keyboard (crkbd) - Keymap

This directory contains a custom keymap for a Corne keyboard (crkbd), rev4.1 standard variant.

### Making Changes

1. Edit relevant keymaps files, e.g. `config/keyboard/corne/keymap.c`
2. Run update script: `cd config/keyboard` then`./keyboard-update.sh`

The script:
- Compiles keymap
- Waits for keyboard to be put in bootloader mode
- Flashes firmware
- Updates visualizations automatically
- Creates a backup of previous keymap

The relevant keymap source files are:

- `config/keyboard/corne/keymap.c` - Main keymap definitions
- `config/keyboard/corne/config.h` - Header configuration
- `config/keyboard/corne/rules.mk` - Build options

## Current Keymap Layout

![Corne Keyboard Layout](crkbd_keymap.png)

## Requirements

The script requires these tools to be installed:
- QMK CLI
- keymap-drawer
- ImageMagick

## Directory Structure

```
config/keyboard/
├── README.md              
├── keyboard-update.sh    # unified update script
├── corne/                # keymap source files
│   ├── keymap.c          # main keymap definitions
│   ├── config.h          # header configuration
│   └── rules.mk          # build options
├── crkbd_keymap.png      # current keyboard visualization
├── crkbd_keymap.svg      # SVG version of visualization
└── backups/              # backups directory
```

## Recovery

To restore a previous keymap:

```bash
# List available backups
ls config/keyboard/backups/

# Restore a specific backup
cp config/keyboard/backups/keymap_c_YYYYMMDD_HHMMSS.backup \
   config/keyboard/corne/keymap.c

# Then run the update script
./keyboard-update.sh
