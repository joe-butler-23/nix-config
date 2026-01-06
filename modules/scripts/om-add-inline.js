#!/usr/bin/env node

const memLib = require('openmemory-js');
const path = require('path');

const DB_PATH = process.env.OM_DB_PATH;
const PROJECT_NAME = process.env.OM_PROJECT || 'global';
const API_KEY = process.env.OPENAI_API_KEY || '';

const content = process.argv.slice(2).join(' ') || '';

if (!content) {
  console.log(JSON.stringify({ error: 'Content required' }));
  process.exit(1);
}

const mem = new memLib.Memory({
  path: DB_PATH,
  embeddings: {
    provider: API_KEY ? 'openai' : 'synthetic',
    apiKey: API_KEY
  }
});

mem.add(content, { metadata: { project: PROJECT_NAME } })
  .then(() => console.log(JSON.stringify({ success: true, message: 'Memory added' })))
  .catch(err => {
    console.log(JSON.stringify({ error: err.message }));
    process.exit(1);
  });
