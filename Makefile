.PHONY: format check clean

# Format all Nix files using treefmt (alejandra, statix, deadnix)
format:
	nix fmt

# Check formatting/linting without modifying files
check:
	nix fmt --check

# Clean up
clean:
	echo "Nothing to clean"
