{
  lib,
  pkgs,
  config,
  ...
}: let
  triageInstructionsMd = ''
    # Triage Instructions

    ## Input Analysis
    Read the contents of `~/documents/projects/refile.org`.
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
          trigger = ":triage";
          replace = triageInstructionsMd;
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
