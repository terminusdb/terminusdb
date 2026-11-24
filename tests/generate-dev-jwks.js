#!/usr/bin/env node
/**
 * Generate a dev RSA key pair for JWT integration tests.
 *
 * Writes two files:
 *   - dashboard/assets/test-jwks.json  (JWKS document served to TerminusDB)
 *   - /tmp/test-jwt-keypair.json       (private key + JWKS for test signing)
 *
 * Usage: node tests/generate-dev-jwks.js
 */
const fs = require('fs')
const path = require('path')
const { jwt } = require('./lib')

const ASSETS_DIR = path.resolve(__dirname, '../dashboard/assets')
const JWKS_FILE = path.join(ASSETS_DIR, 'test-jwks.json')
const KEYPAIR_FILE = '/tmp/test-jwt-keypair.json'

const keyPair = jwt.generateKeyPair('test-jwt-key')

fs.mkdirSync(ASSETS_DIR, { recursive: true })
fs.writeFileSync(JWKS_FILE, JSON.stringify(keyPair.jwks, null, 2))
fs.writeFileSync(KEYPAIR_FILE, JSON.stringify(keyPair))

console.log(`JWKS written to ${JWKS_FILE}`)
console.log(`Key pair written to ${KEYPAIR_FILE}`)
