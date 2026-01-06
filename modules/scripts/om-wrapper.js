#!/usr/bin/env node

const memLib = require('openmemory-js');
const path = require('path');

const DB_PATH = process.env.OM_DB_PATH || path.join(process.env.HOME || process.env.HOME, '.local/share/openmemory/memory.sqlite');
const PROJECT_NAME = process.env.OM_PROJECT || 'global';
const OPENAI_API_KEY = process.env.OPENAI_API_KEY || process.env.OPENMEMORY_API_KEY || '';

async function main() {
  const args = process.argv.slice(2);
  const command = process.argv[2];
  const content = args.slice(1).join(' ') || '';

  const mem = new memLib.Memory({
    path: DB_PATH,
    embeddings: {
      provider: OPENAI_API_KEY ? 'openai' : 'synthetic',
      apiKey: OPENAI_API_KEY
    }
  });

  try {
    switch(command) {
      case 'add':
        await mem.add(content, {
          metadata: { project: PROJECT_NAME }
        });
        console.log(JSON.stringify({ success: true, message: 'Memory added' }));
        break;

      case 'query':
        const results = await mem.query(content, {
          limit: parseInt(process.env.OM_LIMIT || '10')
        });
        console.log(JSON.stringify(results, null, 2));
        break;

      case 'list':
        const allMemories = await mem.getAll({
          limit: parseInt(process.env.OM_LIMIT || '100')
        });
        console.log(JSON.stringify(allMemories, null, 2));
        break;

      case 'delete':
        const id = args[0];
        if (!id) {
          console.log(JSON.stringify({ error: 'Memory ID required' }));
          process.exit(1);
        }
        await mem.delete(id);
        console.log(JSON.stringify({ success: true, message: 'Memory deleted' }));
        break;

      default:
        console.log(JSON.stringify({ error: `Unknown command: ${command}` }));
        process.exit(1);
    }
  } catch(error) {
    console.log(JSON.stringify({ error: error.message }));
    process.exit(1);
  }
}

main();
