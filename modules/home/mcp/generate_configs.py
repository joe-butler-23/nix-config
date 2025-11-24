import json
import os
from pathlib import Path

def generate_opencode_config(servers):
    transformed_servers = {}
    for name, server in servers.items():
        transformed_servers[name] = {
            "type": "local",
            "command": " ".join([server["command"]] + server.get("args", [])),
            "enabled": True,
            "environment": server.get("environment", {})
        }
    return {
        "$schema": "https://opencode.ai/config.json",
        "mcp": transformed_servers
    }

def main():
    mcp_dir = Path(__file__).parent

    with open(mcp_dir / "master-servers.json", "r") as f:
        master_servers = json.load(f)

    with open(mcp_dir / "master-clients.json", "r") as f:
        master_clients = json.load(f)

    servers = master_servers.get("servers", {})
    clients = master_clients.get("clients", {})

    for client_name, client_config in clients.items():
        output_directory = Path(client_config["directory"]).expanduser()
        output_directory.mkdir(parents=True, exist_ok=True)
        output_file_path = output_directory / client_config["fileName"]

        generated_config = {}
        if client_config["format"] == "opencode":
            generated_config = generate_opencode_config(servers)
        elif client_config["format"] == "mcp":
            generated_config = servers # Direct copy for generic mcp format
        else:
            print(f"Warning: Unknown format {client_config["format"]} for client {client_name}")
            continue

        with open(output_file_path, "w") as f:
            json.dump(generated_config, f, indent=2)
        print(f"Generated config for {client_name} at {output_file_path}")

if __name__ == "__main__":
    main()
