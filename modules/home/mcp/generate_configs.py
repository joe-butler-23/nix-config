import json
import os
from pathlib import Path
import subprocess
import argparse # Add this import

def generate_opencode_config(servers):
    transformed_servers = {}
    for name, server in servers.items():
        command_list = [server["command"]] + server.get("args", [])
        transformed_servers[name] = {
            "type": "local",
            "command": command_list,
            "enabled": True,
            "environment": server.get("environment", {})
        }
    return transformed_servers

def main():
    parser = argparse.ArgumentParser(description="Generate MCP client configurations.")
    parser.add_argument("--client", help="Generate config for a specific client.")
    parser.add_argument("--project-dir", help="Root directory of the project for project-specific configs.")
    args = parser.parse_args()

    mcp_dir = Path(__file__).parent

    with open(mcp_dir / "master-servers.json", "r") as f:
        master_servers = json.load(f)

    with open(mcp_dir / "master-clients.json", "r") as f:
        master_clients = json.load(f)

    servers = master_servers.get("servers", {})
    clients = master_clients.get("clients", {})

    # Determine which clients to process based on arguments
    clients_to_process = {}
    if args.client:
        if args.client not in clients:
            print(f"Error: Client '{args.client}' not found in master-clients.json. Exiting.")
            return
        clients_to_process[args.client] = clients[args.client]
    else:
        # If no client specified, process all global clients
        clients_to_process = clients
    
    # This function is now only used by opencode
    opencode_servers_payload = generate_opencode_config(servers)

    for client_name, client_config in clients_to_process.items():
        # Validate project_config_path for project-specific generation
        if args.project_dir and not client_config.get("project_config_path"):
            print(f"Warning: Client '{client_name}' does not have a 'project_config_path' defined. Skipping project-specific generation.")
            continue

        # Determine the output path (global or project-specific)
        if args.project_dir:
            output_directory = Path(args.project_dir)
            output_file_path = output_directory / client_config["project_config_path"]
        else:
            # Global path
            output_directory = Path(client_config["directory"]).expanduser()
            output_file_path = output_directory / client_config["fileName"]
        
        output_directory.mkdir(parents=True, exist_ok=True) # Ensure directory exists

        # Process the client configuration
        process_client_config(client_name, client_config, servers, opencode_servers_payload, output_file_path)

# Helper function to encapsulate client-specific logic
def process_client_config(client_name, client_config, servers, opencode_servers_payload, output_file_path):
    # Client-specific logic
    if client_name == "cline":
        # Simple case: always create from scratch, wrapped in "mcpServers"
        content_to_write = {"mcpServers": servers}
        with open(output_file_path, "w") as f:
            json.dump(content_to_write, f, indent=2)
        print(f"Generated config for {client_name} at {output_file_path}")

    elif client_name == "opencode":
        merge_path = client_config.get("mergePath")
        if not merge_path:
            print(f"Error: {client_name} must have a 'mergePath' defined. Skipping.")
            return # Use return instead of continue since we are in a function

        if not output_file_path.exists():
            # Create a base file.
            base_content = {
                "$schema": "https://opencode.ai/config.json",
                "mcp": opencode_servers_payload
            }
            with open(output_file_path, "w") as f:
                json.dump(base_content, f, indent=2)
            print(f"Generated new config file for {client_name} at {output_file_path}")
        else:
            # File exists, so we merge
            try:
                # Construct jq filter
                jq_filter = f'.{merge_path} |= (. // {{}}) * $new_content'
                schema = "https://opencode.ai/config.json"
                jq_filter += f' | if ."$schema" | not then ."$schema" = "{schema}" else . end'
                
                jq_command = [
                    "jq",
                    "--argjson", "new_content", json.dumps(opencode_servers_payload),
                    jq_filter,
                    str(output_file_path)
                ]
                
                result = subprocess.run(jq_command, capture_output=True, text=True, check=True)
                with open(output_file_path, "w") as f:
                    f.write(result.stdout)
                print(f"Merged config for {client_name} at {output_file_path}")
            except (subprocess.CalledProcessError, json.JSONDecodeError) as e:
                print(f"Error merging config for {client_name}: {e}")
    
    elif client_name == "gemini-cli":
        merge_path = client_config.get("mergePath")
        if not merge_path:
            print(f"Error: {client_name} must have a 'mergePath' defined. Skipping.")
            return # Use return instead of continue

        if not output_file_path.exists():
             print(f"Warning: mergePath specified for {client_name}, but target file "
                  f"{output_file_path} does not exist. Skipping merge. "
                  f"Please ensure the application creates its default config file first.")
             return # Use return instead of continue
        else:
            # File exists, so we merge
            try:
                # Construct jq filter - no opencode transformation, no schema
                jq_filter = f'.{merge_path} |= (. // {{}}) * $new_content'
                
                jq_command = [
                    "jq",
                    # Use the raw servers dict
                    "--argjson", "new_content", json.dumps(servers),
                    jq_filter,
                    str(output_file_path)
                ]
                
                result = subprocess.run(jq_command, capture_output=True, text=True, check=True)
                with open(output_file_path, "w") as f:
                    f.write(result.stdout)
                print(f"Merged config for {client_name} at {output_file_path}")
            except (subprocess.CalledProcessError, json.JSONDecodeError) as e:
                print(f"Error merging config for {client_name}: {e}")

    else:
        print(f"Warning: Unknown client '{client_name}' configured. Skipping.")


if __name__ == "__main__":
    main()
