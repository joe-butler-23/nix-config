export const NixGitSync = async ({ $ }) => {
  return {
    "tool.execute.before": async (input, output) => {
      // Only intercept bash commands
      if (input.tool !== "bash") return;

      const command = output.args.command;
      const triggers = ["nixos-rebuild", "home-manager", "ns", "hs", "nsdry", "hsdry"];

      // Check if the command is a Nix build command
      if (triggers.some(t => command.includes(t))) {
        try {
          // Format code first
          try {
            await $`nix fmt`;
          } catch (e) {
            console.error("Formatting failed:", e);
          }

          // Check for uncommitted changes
          const status = await $`git status --porcelain`.text();
          
          if (status.trim().length > 0) {
            // Stage and commit changes
            await $`git add .`;
            await $`git commit -m "chore: auto-commit before nix build"`;
          }
        } catch (error) {
          // Fail silently or log if git commands fail (e.g. not a git repo)
          console.error("Auto-commit failed:", error);
        }
      }
    },
  };
};
