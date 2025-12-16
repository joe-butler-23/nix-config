{
  lib,
  pkgs,
  config,
  ...
}: let
  triageInstructionsMd = ''
    # Triage Instructions

    ## Input Analysis
    Read the contents of `~/projects/refile.org`.
    If the file is empty, stop and report **"Inbox Zero"**.

    ## Classification Logic (The Sorter)
    For each item, apply the following heuristics to determine the target header.

    ### Categories

    #### Immediate Actions
    **Criteria:** Appointments, time-sensitive emails, blockers, or quick administrative wins taking less than five minutes.

    #### AI Queue
    **Criteria:** Text processing, coding, configuration editing, data formatting, summarisation, or research planning.
    **Constraint:** If you (the AI) can do the task, it belongs here.

    #### Browser Tasks
    **Criteria:** Tasks that require the user to be on their web browser (reading articles, watching videos, online shopping, etc.).

    #### Project
    **Name format:** `Project: [Name]`
    **Criteria:** Deep work or physical actions specific to a domain (for example: Health, House, Sys-Arc).

    ## Transformation (The Refiner)
    Do not copy and paste tasks verbatim. Rewrite each task to be AI-ready. Ask the user clarifying questions if needed.

    ### Examples
    - **Vague:** `fix vim`
      **AI-ready:** Debug `init.el` to resolve startup error regarding recent-files

    - **Vague:** `email nan`
      **AI-ready:** Draft and send email to Nan regarding `[Topic]`

    ## Execution Plan (The Output)
    Before applying any changes, present a table of proposed actions.

    ### Table columns
    - Original Input
    - Target Header
    - Refined Task Name
    - Why?

    **Confirmation:** Wait for user confirmation before proceeding.

    ## Application (On Confirmation)
    1. Append the refined tasks to `tasks.org` under the correct headers.
    2. Clear the processed items from `refile.org`.
    3. *(Optional)* If an item looks useful but is not a task, clarify if the user wants to store it elsewhere.
  '';
in {
  sops.secrets."secrets.yml" = {
    key = "espanso_matches";
    path = "${config.xdg.configHome}/espanso/match/secrets.yml";
  };

  services.espanso = {
    enable = true;
    package = pkgs.espanso-wayland;

    # config/default.yml
    configs.default = {
      toggle_key = "OFF";
      show_notifications = false;
      auto_restart = true;
      backend = "Inject";

      app_specific_configs = [
        {
          filter_class = "kitty";
          config = {
            paste_shortcut = "CTRL+SHIFT+V";
          };
        }
      ];

      keyboard_layout = {
        layout = "gb";
      };
    };

    # match/base.yml
    matches.base = {
      matches = [
        {
          trigger = ":now";
          replace = "{{mytime}}";
          vars = [
            {
              name = "mytime";
              type = "date";
              params = {
                format = "%H:%M";
              };
            }
          ];
        }

        {
          trigger = ":date";
          replace = "{{mydate}}";
          vars = [
            {
              name = "mydate";
              type = "date";
              params = {
                format = "%Y-%m-%d";
              };
            }
          ];
        }

        {
          trigger = ":datetime";
          replace = "{{mydatetime}}";
          vars = [
            {
              name = "mydatetime";
              type = "date";
              params = {
                format = "%Y-%m-%d %H:%M";
              };
            }
          ];
        }

        {
          trigger = ":triage";
          replace = triageInstructionsMd;
        }
      ];
    };

    # match/ai-prompts.yml
    matches.ai-prompts = {
      matches = [
        {
          trigger = ":ai_test_prompt";
          replace = "This is a test prompt to verify Espanso AI integration is working correctly.";
        }

        {
          trigger = ":ai_code_review";
          replace = ''
            Please review the following code for:
            - Correctness and logic errors
            - Performance issues
            - Security vulnerabilities
            - Code style and best practices
            - Missing error handling

            Provide specific suggestions for improvement.
          '';
        }

        {
          trigger = ":ai_debug";
          replace = ''
            I'm encountering an issue with the following code. Please help me:
            1. Identify the root cause of the problem
            2. Explain why it's happening
            3. Suggest a fix with explanation
            4. Recommend how to prevent similar issues

            Code and error:
          '';
        }

        {
          trigger = ":ai_explain";
          replace = ''
            Please explain the following code:
            - What does it do?
            - How does it work?
            - What are the key concepts?
            - Are there any edge cases or gotchas?

            Code:
          '';
        }

        {
          trigger = ":ai_docs";
          replace = ''
            Please generate clear, concise documentation for the following code:
            - Brief description of purpose
            - Parameters and return values
            - Usage examples
            - Any important notes or warnings

            Code:
          '';
        }

        {
          trigger = ":ai_refactor";
          replace = ''
            Please suggest refactoring improvements for the following code:
            - Improve readability
            - Reduce complexity
            - Follow best practices
            - Maintain existing functionality

            Code:
          '';
        }

        {
          trigger = ":ai_test";
          replace = ''
            Please generate comprehensive tests for the following code:
            - Unit tests for core functionality
            - Edge cases and error conditions
            - Mock external dependencies
            - Aim for high coverage

            Code:
          '';
        }
      ];
    };

    # match/shortcuts.yml
    matches.shortcuts = {
      matches = [
        # Symbols and Unicode
        {
          trigger = ":shrug";
          replace = "¯\\_(ツ)_/¯";
        }

        {
          trigger = ":check";
          replace = "✓";
        }

        {
          trigger = ":cross";
          replace = "✗";
        }

        {
          trigger = ":arrow";
          replace = "→";
        }

        {
          trigger = ":larrow";
          replace = "←";
        }

        # Common phrases
        {
          trigger = ":lgtm";
          replace = "Looks good to me!";
        }

        {
          trigger = ":wip";
          replace = "Work in progress";
        }

        {
          trigger = ":todo";
          replace = "TODO: ";
        }

        {
          trigger = ":fixme";
          replace = "FIXME: ";
        }

        # Git shortcuts
        {
          trigger = ":gitfix";
          replace = "fix: ";
        }

        {
          trigger = ":gitfeat";
          replace = "feat: ";
        }

        {
          trigger = ":gitdocs";
          replace = "docs: ";
        }

        {
          trigger = ":gitrefactor";
          replace = "refactor: ";
        }

        {
          trigger = ":gitchore";
          replace = "chore: ";
        }
      ];
    };
  };

  systemd.user.services.espanso = {
    Unit = {
      After = ["graphical-session.target" "sops-nix.service"];
      PartOf = ["graphical-session.target"];
      Wants = ["sops-nix.service"];
    };

    Install.WantedBy = lib.mkForce ["graphical-session.target"];
  };
}
