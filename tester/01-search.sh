#!/usr/bin/env bash

# In case of system uses a non-POSIX shell, like fish or nushell,
# we want to ensure run also our forked processes in a bash environment.
SHELL="bash"

# === Change keybinds or add more here ===

declare -a INDEXES=(
    "nixpkgs ctrl-n"
    "home-manager ctrl-h"

    # you can add any indexes combination here,
    # like `nixpkgs,nixos`

    "all ctrl-a"
)

SEARCH_SNIPPET_KEY="ctrl-w"
OPEN_SOURCE_KEY="ctrl-s"
OPEN_HOMEPAGE_KEY="ctrl-o"
NIX_SHELL_KEY="ctrl-i"
PRINT_PREVIEW_KEY="ctrl-p"
SELECT_PACKAGE_KEY="ctrl-x"

OPENER="xdg-open"

if [[ "$(uname)" == 'Darwin' ]]; then
    SEARCH_SNIPPET_KEY="alt-w"
    OPEN_SOURCE_KEY="alt-s"
    OPEN_HOMEPAGE_KEY="alt-o"
    NIX_SHELL_KEY="alt-i"
    PRINT_PREVIEW_KEY="alt-p"
    SELECT_PACKAGE_KEY="alt-enter"

    OPENER="open"
fi

# ========================================

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to detect package type based on index
detect_package_type() {
    local index="$1"
    local package="$2"
    
    case "$index" in
        "home-manager")
            echo "home-manager"
            ;;
        "nixos")
            echo "system"
            ;;
        "nixpkgs")
            # For nixpkgs, use heuristic-based detection
            if [[ "$package" =~ (server|daemon|service|admin|system|nginx|apache|docker|postgres|mysql|redis|mongo|kubernetes|virtualbox|qemu|libvirt|firewall|iptables|fail2ban|logrotate|cron) ]]; then
                echo "system"
            else
                echo "home-manager"
            fi
            ;;
        *)
            # Default to home-manager for unknown indexes
            echo "home-manager"
            ;;
    esac
}

# Function to save selected package info for next phase
save_package_info() {
    local selected_entry="$1"
    local current_index="$2"
    
    # Extract package name from entry
    local package_name
    package_name=$(echo "$selected_entry" | awk '{ if ($2) { print $2 } else print $1 }')
    
    # Detect package type
    local package_type
    package_type=$(detect_package_type "$current_index" "$package_name")
    
    # Save package info for next phase
    echo "$package_name|$package_type|$current_index|$selected_entry" > "$(dirname "$0")/test-data/selected_package.txt"
    
    print_success "Package '$package_name' ($package_type) selected for installation"
    
    if [[ "$package_type" == "system" ]]; then
        print_info "This is a system package - will be added to system configuration"
    else
        print_info "This is a user package - will be added to home-manager configuration"
    fi
    
    print_info "You can now run Phase 2 to add it to your configuration"
}

# for debug / development
CMD="${NIX_SEARCH_TV:-nix-search-tv}"

# bind_index binds given $key to given $index
bind_index() {
    local key="$1"
    local index="$2"

    local prompt=""
    local indexes_flag=""
    if [[ -n "$index" && "$index" != "all" ]]; then
        indexes_flag="--indexes $index"
        prompt=$index
    fi

    local preview="$CMD preview $indexes_flag"
    local print="$CMD print $indexes_flag"

    echo "$key:change-prompt($prompt> )+change-preview($preview {})+reload($print)"
}

STATE_FILE="/tmp/nix-search-tv-fzf"

# save_state saves currently displayed index
# to $STATE_FILE. This file serves as an external script state
# for communication between "print" and "preview" commands
save_state() {
    local index="$1"

    local indexes_flag=""
    if [[ -n "$index" && "$index" != "all" ]]; then
        indexes_flag="--indexes $index"
    fi

    echo "execute(echo $indexes_flag > $STATE_FILE)"
}

HEADER="$OPEN_HOMEPAGE_KEY - open homepage
$OPEN_SOURCE_KEY - open source
$SEARCH_SNIPPET_KEY - search github for snippets
$NIX_SHELL_KEY - nix-shell
$PRINT_PREVIEW_KEY - print preview
$SELECT_PACKAGE_KEY - select package for installation
"

FZF_BINDS=""
for e in "${INDEXES[@]}"; do
    index=$(echo "$e" | awk '{ print $1 }')
    keybind=$(echo "$e" | awk '{ print $2 }')

    fzf_bind=$(bind_index "$keybind" "$index")
    fzf_save_state=$(save_state "$index")
    FZF_BINDS="$FZF_BINDS --bind '$fzf_bind+$fzf_save_state'"

    newline=$'\n'
    HEADER="$HEADER$keybind - $index$newline"
done

# reset state
echo "" >/tmp/nix-search-tv-fzf

SEARCH_SNIPPET_CMD=$'echo "{}"'
# fzf surrounds by matched package with ', trim them
SEARCH_SNIPPET_CMD="$SEARCH_SNIPPET_CMD | tr -d \"\'\" "
# if it's multi-index search, then we need to remote prefix
SEARCH_SNIPPET_CMD="$SEARCH_SNIPPET_CMD | awk \'{ if (\$2) { print \$2 } else print \$1 }\' "
SEARCH_SNIPPET_CMD="$SEARCH_SNIPPET_CMD | xargs printf \"https://github.com/search?type=code&q=lang:nix+%s\" \$1 "

NIX_SHELL_CMD='nix-shell --run $SHELL -p $(echo "{}" | sed "s:nixpkgs/::g"'
NIX_SHELL_CMD="$NIX_SHELL_CMD | tr -d \"\'\")"

# Custom command for selecting package
SELECT_PACKAGE_CMD='echo "{}" | xargs -I {} bash -c "current_index=$(cat '$STATE_FILE'); save_package_info \"{}\" \"$current_index\""'

PREVIEW_WINDOW="wrap"
[ "$(tput cols)" -lt 90 ] && PREVIEW_WINDOW="$PREVIEW_WINDOW,up"

print_info "Nix Package Search - Phase 1 (Official Enhanced Version)"
echo "======================================================"
print_info "Loading package list... (this may take a moment)"

# Run the official nix-search-tv with our custom selection functionality
eval "$CMD print | fzf \
    --preview '$CMD preview \$(cat $STATE_FILE) {}' \
    --bind '$OPEN_SOURCE_KEY:execute($CMD source \$(cat $STATE_FILE) {} | xargs $OPENER)' \
    --bind '$OPEN_HOMEPAGE_KEY:execute($CMD homepage \$(cat $STATE_FILE) {} | xargs $OPENER)' \
    --bind $'$SEARCH_SNIPPET_KEY:execute($SEARCH_SNIPPET_CMD | xargs $OPENER)' \
    --bind $'$NIX_SHELL_KEY:become($NIX_SHELL_CMD)' \
    --bind $'$PRINT_PREVIEW_KEY:become($CMD preview \$(cat $STATE_FILE) {})' \
    --bind '$SELECT_PACKAGE_KEY:execute-silent($SELECT_PACKAGE_CMD)+abort' \
    --layout reverse \
    --scheme history \
    --preview-window='$PREVIEW_WINDOW' \
    --header '$HEADER' \
    --header-first \
    --header-border \
    --header-label \"Help\" \
    $FZF_BINDS
"

# Check if package was selected
if [[ -f "$(dirname "$0")/test-data/selected_package.txt" ]]; then
    print_success "Phase 1 complete. Ready for Phase 2."
else
    print_info "Phase 1 cancelled - no package selected."
    exit 1
fi
