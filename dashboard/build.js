#!/usr/bin/env node

/**
 * Builds the static Scalar dashboard:
 * 1. Converts docs/openapi.yaml → dashboard/src/assets/openapi.json
 * 2. Rewrites the server URL to be relative (/api) so it works on any host
 */

const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');

const ROOT = path.resolve(__dirname, '..');
const SPEC_SRC = path.join(ROOT, 'docs', 'openapi.yaml');
const ASSETS_DIR = path.join(__dirname, 'src', 'assets');
const SPEC_OUT = path.join(ASSETS_DIR, 'openapi.json');

function main() {
  // Read and parse the OpenAPI YAML
  const yamlText = fs.readFileSync(SPEC_SRC, 'utf8');
  const spec = yaml.load(yamlText);

  // Rewrite server URLs to be relative so Scalar uses the same origin
  if (spec.servers) {
    spec.servers = spec.servers.map((s) => ({
      ...s,
      url: '/api',
      description: 'Current TerminusDB server',
    }));
  }

  // Ensure assets directory exists
  fs.mkdirSync(ASSETS_DIR, { recursive: true });

  // Write JSON spec
  const json = JSON.stringify(spec, null, 2);
  fs.writeFileSync(SPEC_OUT, json, 'utf8');

  const sizeKB = Math.round(Buffer.byteLength(json, 'utf8') / 1024);
  console.log(`Built openapi.json (${sizeKB} KB) → ${path.relative(ROOT, SPEC_OUT)}`);
}

main();
