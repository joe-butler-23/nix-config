{pkgs}:
pkgs.writeShellScriptBin "weekly-review" ''
  set -e

  echo "========================================"
  echo " 🛠️  Weekly Maintenance & Review"
  echo "========================================"

  # 1. Update Flake Inputs
  echo -e "\n📦 Updating Flake Inputs..."
  CONFIG_DIR="$HOME/nix-config"

  if [ -d "$CONFIG_DIR" ]; then
    cd "$CONFIG_DIR"

    # Update the lockfile
    nix flake update

    # Check if flake.lock has changed
    if git diff --quiet flake.lock; then
      echo "✅ flake.lock is already up to date."
    else
      echo "🔍 Running flake check..."
      nix flake check

      echo "💾 Committing and pushing changes..."
      git add flake.lock
      git commit -m "chore: update flake.lock"
      git push
      echo "✅ Changes pushed to remote."
    fi
  else
    echo "❌ Config directory $CONFIG_DIR not found!"
  fi

  # 2. File Review
  echo -e "\n📂 Recent Files (Last 7 Days)..."
  echo "   Searching in: development, boox, Documents, Downloads, Pictures, utilities"
  echo "----------------------------------------"

  # Define directories to scan
  TARGET_DIRS=()
  for dir in "development" "boox" "Documents" "Downloads" "Pictures" "utilities"; do
      if [ -d "$HOME/$dir" ]; then
          TARGET_DIRS+=("$HOME/$dir")
      fi
  done

  if [ ''${#TARGET_DIRS[@]} -eq 0 ]; then
      echo "⚠️  No target directories found!"
  else
      ${pkgs.fd}/bin/fd . "''${TARGET_DIRS[@]}" \
          --type f \
          --changed-within 7d \
          --exclude ".git" \
          --exclude "node_modules" \
          --exclude ".cache" \
          --exclude ".env" \
          --exclude ".venv" \
          --color always
  fi

  echo -e "\n========================================"
  echo "📝 Review complete."
''
