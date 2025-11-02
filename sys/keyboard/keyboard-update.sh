#!/bin/bash

# Unified keyboard configuration update script
# Usage: ./keyboard-update.sh [path_to_keymap.c]
# If no argument provided, uses existing keymap.c

set -e  # Exit on any error

# Configuration
KEYBOARD_DIR="$HOME/nix-config/sys/keyboard/"
CORNE_DIR="$KEYBOARD_DIR/corne"
KEYMAP_FILE="keymap.c"
BACKUP_DIR="$DOTFILES_DIR/backups"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to wait for keyboard in bootloader mode
wait_for_keyboard() {
    print_status "Waiting for keyboard in bootloader mode..."
    print_status "Put your Corne keyboard into bootloader mode (layer 3 key)"
    print_status "The script will automatically detect when it's ready"
    
    while true; do
        if lsusb | grep -q "atmel.*dfu" || lsusb | grep -q "03eb.*2ff4"; then
            print_success "Keyboard detected in bootloader mode!"
            break
        fi
        sleep 1
    done
}

# Create backup directory if it doesn't exist
mkdir -p "$BACKUP_DIR"

# Handle input keymap file
if [ $# -eq 1 ]; then
    INPUT_KEYMAP="$1"
    
    if [ ! -f "$INPUT_KEYMAP" ]; then
        print_error "Input keymap file not found: $INPUT_KEYMAP"
        exit 1
    fi
    
    print_status "Using provided keymap file: $INPUT_KEYMAP"
    
    # Backup current keymap
    print_status "Backing up current keymap..."
    cp "$CORNE_DIR/$KEYMAP_FILE" "$BACKUP_DIR/${KEYMAP_FILE}_${TIMESTAMP}.backup"
    
    # Copy new keymap
    print_status "Copying new keymap to corne directory..."
    cp "$INPUT_KEYMAP" "$CORNE_DIR/$KEYMAP_FILE"
else
    print_status "Using existing keymap.c in corne directory"
    
    # Still create backup
    print_status "Creating backup of current keymap..."
    cp "$CORNE_DIR/$KEYMAP_FILE" "$BACKUP_DIR/${KEYMAP_FILE}_${TIMESTAMP}.backup"
fi

# Navigate to corne directory
cd "$CORNE_DIR" || {
    print_error "Could not change to corne directory: $CORNE_DIR"
    exit 1
}

# Check required commands
if ! command_exists qmk; then
    print_error "QMK CLI not found. Please install QMK firmware first."
    exit 1
fi

if ! command_exists keymap; then
    print_error "keymap-drawer not found. Please install with: pipx install keymap-drawer"
    exit 1
fi

if ! command_exists convert; then
    print_error "ImageMagick convert not found. Please install ImageMagick."
    exit 1
fi

# Clean previous builds
print_status "Cleaning previous builds..."
make clean

# Compile keymap
print_status "Compiling keymap..."
if qmk compile -kb crkbd/rev4_1/standard -km joebutler23; then
    print_success "Compilation successful!"
else
    print_error "Compilation failed!"
    exit 1
fi

# Wait for keyboard and flash
wait_for_keyboard

print_status "Flashing firmware..."
if qmk flash -kb crkbd/rev4_1/standard -km joebutler23; then
    print_success "Firmware flashed successfully!"
else
    print_error "Flashing failed!"
    exit 1
fi

# Update visualizations
print_status "Updating visualizations..."

# Generate YAML from keymap.c
print_status "Generating YAML from keymap.c..."
qmk c2json "$CORNE_DIR/$KEYMAP_FILE" | keymap parse -c 10 -q - > "$CORNE_DIR/crkbd_joebutler23.yaml"

# Generate SVG from YAML
print_status "Generating SVG from YAML..."
keymap draw "$CORNE_DIR/crkbd_joebutler23.yaml" > "$CORNE_DIR/crkbd_joebutler23.ortho.svg"

# Convert SVG to PNG
print_status "Converting SVG to PNG..."
convert "$CORNE_DIR/crkbd_joebutler23.ortho.svg" "$DOTFILES_DIR/crkbd_keymap.png"

# Copy the SVG to dotfiles directory
print_status "Copying files to dotfiles directory..."
cp "$CORNE_DIR/crkbd_joebutler23.ortho.svg" "$DOTFILES_DIR/crkbd_keymap.svg"

# Clean up temporary YAML file
rm -f "$CORNE_DIR/crkbd_joebutler23.yaml"

print_success "Keyboard configuration updated successfully!"
echo ""
print_status "Summary of changes:"
echo "  - Keymap compiled and flashed"
echo "  - Visualizations updated (SVG and PNG)"
echo "  - Backup created: $BACKUP_DIR/${KEYMAP_FILE}_${TIMESTAMP}.backup"
echo ""
print_status "Updated files:"
echo "  - $DOTFILES_DIR/crkbd_keymap.png"
echo "  - $DOTFILES_DIR/crkbd_keymap.svg"
echo ""
print_success "Process completed! Your keyboard is ready to use."
