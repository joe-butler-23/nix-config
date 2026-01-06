{pkgs}: let
  omCli = pkgs.writeText "om-cli.js" ''
    const path = require("path");

    const base = "${pkgs.openmemory-js}/libexec/package";
    const { Memory } = require(path.join(base, "dist", "index.js"));
    const db = require(path.join(base, "dist", "core", "db.js"));
    const { all_async, get_async, run_async, vector_store } = db;

    const op = process.argv[2];
    const args = process.argv.slice(3);

    const project = process.env.OM_PROJECT || "global";
    const limitRaw = process.env.OM_LIMIT || "10";
    const limit = Number.parseInt(limitRaw, 10) || 10;
    const jsonOut = process.env.OM_JSON === "1";
    const category = (process.env.OM_CATEGORY || "").trim();
    const rawTags = (process.env.OM_TAGS || "").trim();

    const parseTags = (val) => val
      .split(",")
      .map((tag) => tag.trim())
      .filter((tag) => tag.length > 0);

    const filterTags = [];
    if (category) filterTags.push(category);
    if (rawTags) filterTags.push(...parseTags(rawTags));
    const uniqueTags = Array.from(new Set(filterTags));

    const normalizeTags = (val) => {
      if (!val) return [];
      if (Array.isArray(val)) return val.filter(Boolean);
      if (typeof val === "string") {
        try {
          const parsed = JSON.parse(val);
          if (Array.isArray(parsed)) return parsed.filter(Boolean);
        } catch (err) {
          return parseTags(val);
        }
        return parseTags(val);
      }
      return [];
    };

    const normalizeMem = (m) => {
      const tags = normalizeTags(m.tags);
      let meta = m.meta;
      if (typeof meta === "string") {
        try {
          meta = JSON.parse(meta);
        } catch (err) {
          meta = {};
        }
      }
      return { ...m, tags, meta };
    };

    const matchesTags = (m) => {
      if (uniqueTags.length === 0) return true;
      const tags = normalizeTags(m.tags);
      return uniqueTags.every((tag) => tags.includes(tag));
    };

    const print = (payload) => {
      if (jsonOut) {
        console.log(JSON.stringify(payload, null, 2));
        return;
      }
      if (Array.isArray(payload)) {
        if (payload.length === 0) {
          console.log("No memories found.");
          return;
        }
        payload.forEach((m, idx) => {
          console.log(String(idx + 1) + ". " + m.content);
          console.log("   id: " + m.id);
          if (m.primary_sector) console.log("   sector: " + m.primary_sector);
          if (typeof m.score === "number") console.log("   score: " + m.score.toFixed(3));
          if (typeof m.salience === "number") console.log("   salience: " + m.salience.toFixed(3));
          if (m.tags && m.tags.length) console.log("   tags: " + m.tags.join(", "));
          console.log("");
        });
        return;
      }
      if (payload && typeof payload === "object") {
        console.log(JSON.stringify(payload, null, 2));
        return;
      }
      console.log(payload);
    };

    const requireText = (label) => {
      const text = args.join(" ").trim();
      if (text.length === 0) {
        console.error(label + " required.");
        process.exit(2);
      }
      return text;
    };

    const addMemory = async () => {
      const content = requireText("content");
      const mem = new Memory(project);
      const tags = uniqueTags;
      const res = await mem.add(content, tags.length ? { tags } : {});
      print(res);
    };

    const queryMemory = async () => {
      const query = requireText("query");
      const mem = new Memory(project);
      const results = await mem.search(query, { limit });
      const items = results.map(normalizeMem).filter(matchesTags);
      print(items);
    };

    const listMemory = async () => {
      const rows = await all_async(
        "select * from memories where user_id=? order by created_at desc limit ? offset 0",
        [project, limit],
      );
      const items = rows.map(normalizeMem).filter(matchesTags);
      print(items);
    };

    const statsMemory = async () => {
      const rows = await all_async(
        "select primary_sector as sector, count(*) as count, avg(salience) as avg_salience from memories where user_id=? group by primary_sector",
        [project],
      );
      const totalRow = await get_async(
        "select count(*) as total from memories where user_id=?",
        [project],
      );
      const payload = {
        project,
        total: totalRow ? totalRow.total : 0,
        sectors: rows.map((row) => ({
          sector: row.sector,
          count: row.count,
          avg_salience: row.avg_salience,
        })),
      };
      print(payload);
    };

    const deleteMemory = async () => {
      const id = args[0];
      if (!id) {
        console.error("id required.");
        process.exit(2);
      }
      const existing = await get_async("select id, user_id from memories where id=?", [id]);
      if (!existing) {
        console.error("memory not found.");
        process.exit(1);
      }
      if (existing.user_id !== project) {
        console.error("memory not in current project.");
        process.exit(1);
      }
      await vector_store.deleteVectors(id);
      await run_async("delete from waypoints where src_id=? or dst_id=?", [id, id]);
      await run_async("delete from memories where id=?", [id]);
      print({ ok: true, id });
    };

    const run = async () => {
      switch (op) {
        case "add":
          await addMemory();
          return;
        case "query":
          await queryMemory();
          return;
        case "list":
          await listMemory();
          return;
        case "stats":
          await statsMemory();
          return;
        case "delete":
          await deleteMemory();
          return;
        default:
          console.error("unknown command.");
          process.exit(2);
      }
    };

    run().catch((err) => {
      console.error("[error] " + (err && err.message ? err.message : String(err)));
      process.exit(1);
    });
  '';

  mkOmScript = {
    name,
    op,
    category ? "",
  }:
    pkgs.writeShellScriptBin name ''
      set -euo pipefail

      MEMORY_DIR="''${XDG_DATA_HOME:-$HOME/.local/share}/openmemory"
      DB_PATH="$MEMORY_DIR/memory.sqlite"
      mkdir -p "$MEMORY_DIR"

      PROJECT_NAME="global"
      if ${pkgs.git}/bin/git rev-parse --show-toplevel >/dev/null 2>&1; then
        PROJECT_NAME="$(${pkgs.coreutils}/bin/basename "$(${pkgs.git}/bin/git rev-parse --show-toplevel)")"
      fi

      OM_PROJECT="$PROJECT_NAME"
      OM_LIMIT="10"
      OM_JSON="0"
      OM_TAGS=""
      OM_CATEGORY="${category}"

      ARGS=()
      while [ "$#" -gt 0 ]; do
        case "$1" in
          --project)
            if [ -n "''${2:-}" ]; then
              OM_PROJECT="$2"
              shift 2
            else
              echo "Missing value for --project" >&2
              exit 2
            fi
            ;;
          --limit)
            if [ -n "''${2:-}" ]; then
              OM_LIMIT="$2"
              shift 2
            else
              echo "Missing value for --limit" >&2
              exit 2
            fi
            ;;
          --tags)
            if [ -n "''${2:-}" ]; then
              OM_TAGS="$2"
              shift 2
            else
              echo "Missing value for --tags" >&2
              exit 2
            fi
            ;;
          --json)
            OM_JSON="1"
            shift
            ;;
          --)
            shift
            ARGS+=("$@")
            break
            ;;
          *)
            ARGS+=("$1")
            shift
            ;;
        esac
      done

      export OM_DB_PATH="$DB_PATH"
      export OM_PROJECT OM_LIMIT OM_JSON OM_TAGS OM_CATEGORY
      export OM_TIER="''${OM_TIER:-smart}"
      export OM_EMBEDDINGS="''${OM_EMBEDDINGS:-synthetic}"

      exec ${pkgs.nodejs}/bin/node ${omCli} ${op} "''${ARGS[@]}"
    '';
in {
  inherit mkOmScript;
}
