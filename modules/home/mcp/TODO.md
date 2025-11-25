## Current Status & Next Steps

So far, we have established the framework for managing MCP server configurations by creating `README.md` and `TODO.md` within this directory. We have successfully integrated the `anki-mcp-server` into the Home Manager configuration. This involved correcting the package type, iteratively debugging the build process, and implementing an imperative script within a Nix wrapper to handle its cloning, dependency installation, and execution. The `anki-mcp-wrapper` command is now available and should function as expected.

**Next steps:** We will now proceed with the remaining tasks outlined below to implement project-specific configurations, starting with enhancing `master-clients.json`.

# MCP Configuration TODO

## Objective

The goal is to extend the current MCP configuration system to support **project-specific configurations**. And to come up with a flexible and modular approach to configuring AI/LLM tooling, that alllows us to easily generate MCP configurations for different clients (i.e. Gemini, Claude, Opencode, Cline) at different levels (i.e. system, user, project), in accordance with the different approaches and schemas that these require. 

Currently, our system declaratively manages a single, global set of MCP server settings for the user. However, tools like Gemini and Opencode also support project-level configuration files (e.g., `.gemini/settings.json` or `opencode.json` in a project's root). This allows for different server settings or models to be used on a per-project basis.

As detailed in the README.md, there is a slightly complex approach here that uses SOPS, wrappers, and a Python script, to make it possible to keep a central store of MCPs, from which the various configurations can be generated. 

This TODO list outlines the tasks required to implement a flexible, on-demand workflow for "stamping" a project with these specific MCP configurations, while still maintaining the central, declarative management for the global setup.

## Tasks

- [ ] **Enhance `master-clients.json`**
  - [ ] Add a `project_config_path` key to each client object. This key will define the relative path and filename for project-specific configs (e.g., `"project_config_path": ".gemini/settings.json"` for the `gemini` client).

- [ ] **Update `generate_configs.py`**
  - [ ] Implement command-line argument parsing (e.g., using Python's `argparse` module).
  - [ ] Add a `--client` argument to specify which client config to generate.
  - [ ] Add a `--project-dir` argument to specify the target project's root directory.
  - [ ] Add logic to differentiate between global and project generation. If `--project-dir` is passed, the script should write the config to that location instead of the default home directory path.
  - [ ] Ensure the script creates the target directory path (e.g., `.gemini/`) if it does not already exist within the project.

- [ ] **Create `mcp-project-init` Wrapper Script**
  - [ ] Create a new, user-friendly shell script in the main `/scripts` directory.
  - [ ] This script should accept a client name as its first argument (e.g., `mcp-project-init gemini`).
  - [ ] It should call `generate_configs.py` with the correct arguments, using the current working directory as the project path (e.g., `python .../generate_configs.py --client $1 --project-dir .`).
  - [ ] Expose this new script to the user's shell environment by adding it to `home.packages` in a relevant Nix file (e.g., `modules/home/packages.nix`).

- [ ] **Update Documentation**
  - [ ] Once the implementation is complete, update `modules/home/mcp/README.md` with a new section explaining how to use the `mcp-project-init` script for generating project-specific configurations.
