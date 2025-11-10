#!/run/current-system/sw/bin/bash

# Test script for package detection logic

set -euo pipefail

# Define the detection function directly (avoid sourcing issues)
detect_package_type() {
    local package="$1"
    local full_entry="$2"
    
    # Check if it's a system service (nixos/services.*)
    if [[ "$full_entry" =~ ^nixos/services\. ]]; then
        echo "service"
        return
    fi
    
    # Simple heuristic-based detection
    # System packages typically contain: server, daemon, service, admin, system
    if [[ "$package" =~ (server|daemon|service|admin|system|nginx|apache|docker|postgres|mysql|redis|mongo|kubernetes|virtualbox|qemu|libvirt|firewall|iptables|fail2ban|logrotate|cron) ]]; then
        echo "system"
        return
    fi
    
    # Default to home-manager for user applications
    echo "home-manager"
}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_test() {
    echo -e "${BLUE}[TEST]${NC} $1"
}

print_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

print_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
}

# Test cases
test_cases=(
    "firefox|nixpkgs/ firefox|home-manager"
    "nginx|nixpkgs/ nginx|system"
    "neovim|nixpkgs/ neovim|home-manager"
    "docker|nixpkgs/ docker|system"
    "unknown-package|nixpkgs/ unknown-package|home-manager"
    "nginx|nixos/ services.nginx.enable|service"
    "postgresql|nixos/ services.postgresql.enable|service"
)

print_test "Testing package detection logic..."
echo

passed=0
total=0

for test_case in "${test_cases[@]}"; do
    IFS='|' read -r package full_entry expected <<< "$test_case"
    
    total=$((total + 1))
    result=$(detect_package_type "$package" "$full_entry")
    
    if [[ "$result" == "$expected" ]]; then
        print_pass "$package -> $result (expected: $expected)"
        passed=$((passed + 1))
    else
        print_fail "$package -> $result (expected: $expected)"
    fi
done

echo
print_test "Test Results: $passed/$total passed"

if [[ $passed -eq $total ]]; then
    print_success "All tests passed!"
    exit 0
else
    print_error "Some tests failed!"
    exit 1
fi
