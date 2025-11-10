# Nix Package Installation Script

## Overview
A sequential script system for installing Nix packages with the following workflow:
1. Search for packages using nix-search-tv
2. Add to the correct packages.nix file
3. Run nixos-rebuild or home-manager switch

## Phase 1: Package Search
The first phase focuses on implementing robust package search functionality.

### Files
- `01-search.sh` - Package search script
- `test-data/` - Temporary storage for testing

### Usage
```bash
cd tester
./01-search.sh
```

## Development Notes
This is a testing environment. The entire `tester/` folder can be safely deleted if the project is abandoned.
