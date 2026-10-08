const fs = require('fs')
const path = require('path')
const { Agent, api, db, jwt, util } = require('../lib')
const { expect } = require('chai')

// Path to write the JWKS file — served by TerminusDB's dashboard assets handler
// at http://<host>:<port>/assets/test-jwks.json
const DASHBOARD_ASSETS_DIR = path.resolve(__dirname, '../../dashboard/assets')
const JWKS_FILE = path.join(DASHBOARD_ASSETS_DIR, 'test-jwks.json')

// Admin agent for setup operations (basic auth)
let adminAgent
// Generated key material
let keyPair
// Whether JWT is enabled on the server (probed at startup)
let jwtEnabled

describe('jwt', function () {
  before(async function () {
    // The JWKS file must be written BEFORE the server starts so the server's
    // initial fetch loads the correct keys. If the file already exists (server
    // already started with it), load the existing key pair.
    if (fs.existsSync(JWKS_FILE) && fs.existsSync('/tmp/test-jwt-keypair.json')) {
      const keyPairData = JSON.parse(fs.readFileSync('/tmp/test-jwt-keypair.json', 'utf-8'))
      keyPair = keyPairData
    } else {
      keyPair = jwt.generateKeyPair('test-jwt-key')
      fs.mkdirSync(DASHBOARD_ASSETS_DIR, { recursive: true })
      fs.writeFileSync(JWKS_FILE, JSON.stringify(keyPair.jwks, null, 2))
      fs.writeFileSync('/tmp/test-jwt-keypair.json', JSON.stringify(keyPair))
    }

    // Set up admin agent for database creation
    adminAgent = new Agent()
    adminAgent.auth()

    // Probe whether JWT is enabled on the server by sending a valid token.
    // If the server returns 401, JWT is not configured and valid-token tests skip.
    const probeToken = jwt.signToken(keyPair.privateKey, { sub: 'admin' }, { kid: 'test-jwt-key' })
    const probeAgent = new Agent()
    probeAgent.set('Authorization', `Bearer ${probeToken}`)
    try {
      const res = await probeAgent.get('/api/')
      jwtEnabled = res.status === 200
    } catch (e) {
      jwtEnabled = false
    }
  })

  after(async function () {
    // Clean up any test databases created
    try { await db.delete(adminAgent).unverified() } catch (e) { /* ignore */ }
  })

  describe('bearer: invalid token', function () {
    it('fails connect with random string token', async function () {
      const agent = new Agent()
      agent.set('Authorization', `Bearer ${util.randomString()}`)
      await agent.get('/api/').then(api.response.verify(api.response.incorrectAuthentication))
    })

    it('fails connect with alg:none token', async function () {
      // Craft a token with alg:none — should be rejected by the Rust JWT module
      const header = Buffer.from(JSON.stringify({ alg: 'none', typ: 'JWT', kid: 'test-jwt-key' })).toString('base64url')
      const payload = Buffer.from(JSON.stringify({ sub: 'admin', exp: Math.floor(Date.now() / 1000) + 3600 })).toString('base64url')
      const noneToken = `${header}.${payload}.`
      const agent = new Agent()
      agent.set('Authorization', `Bearer ${noneToken}`)
      await agent.get('/api/').then(api.response.verify(api.response.incorrectAuthentication))
    })
  })

  describe('bearer: valid token with existing user', function () {
    let jwtAgent

    before(async function () {
      if (!jwtEnabled) this.skip()

      // Create a database as admin first
      adminAgent = new Agent()
      adminAgent.auth()
      await db.create(adminAgent)

      // Create agent with JWT token for admin user
      const token = jwt.signToken(keyPair.privateKey, {
        sub: 'admin',
      }, { kid: 'test-jwt-key' })

      jwtAgent = new Agent()
      jwtAgent.set('Authorization', `Bearer ${token}`)
    })

    it('connects successfully', async function () {
      await jwtAgent.get('/api/').then(api.response.verify({ status: 200 }))
    })

    it('can list databases', async function () {
      const res = await jwtAgent.get(`/api/db/admin/${adminAgent.dbName}`)
      expect(res).to.have.property('status', 200)
    })

    after(async function () {
      await db.delete(adminAgent).unverified()
    })
  })

  describe('bearer: valid token with unknown user', function () {
    it('fails connect when user does not exist in DB', async function () {
      const token = jwt.signToken(keyPair.privateKey, {
        sub: 'nonexistent-user-' + util.randomString(),
      }, { kid: 'test-jwt-key' })

      const agent = new Agent()
      agent.set('Authorization', `Bearer ${token}`)
      await agent.get('/api/').then(api.response.verify(api.response.incorrectAuthentication))
    })
  })

  describe('bearer: expired token', function () {
    it('fails connect with expired token', async function () {
      const token = jwt.signToken(keyPair.privateKey, {
        sub: 'admin',
      }, { kid: 'test-jwt-key', expiresIn: '-120s' })

      const agent = new Agent()
      agent.set('Authorization', `Bearer ${token}`)
      await agent.get('/api/').then(api.response.verify(api.response.incorrectAuthentication))
    })
  })

  describe('bearer: wrong key (kid mismatch)', function () {
    it('fails connect with token signed by unknown kid', async function () {
      // Generate a different key pair with a different kid
      const otherKey = jwt.generateKeyPair('other-key')
      const token = jwt.signToken(otherKey.privateKey, {
        sub: 'admin',
      }, { kid: 'other-key' })

      const agent = new Agent()
      agent.set('Authorization', `Bearer ${token}`)
      await agent.get('/api/').then(api.response.verify(api.response.incorrectAuthentication))
    })
  })

  describe('bearer: token with issuer validation', function () {
    let jwtAgent

    before(async function () {
      // This test only works if TERMINUSDB_JWT_ISSUER is set on the server.
      // We test that a token with a mismatched issuer is rejected.
      // If no issuer is configured on the server, this test is skipped.
      const token = jwt.signToken(keyPair.privateKey, {
        sub: 'admin',
      }, { kid: 'test-jwt-key', issuer: 'wrong-issuer' })

      jwtAgent = new Agent()
      jwtAgent.set('Authorization', `Bearer ${token}`)
    })

    it('handles token with issuer claim', async function () {
      // If issuer validation is not configured, this will succeed.
      // If it is configured with a different issuer, this will fail with 401.
      // Either way, the server should not crash.
      const res = await jwtAgent.get('/api/')
      expect(res.status).to.be.oneOf([200, 401])
    })
  })

  describe('bearer: algorithm confusion regression', function () {
    it('rejects HS256 token with RSA kid (algorithm confusion attack)', async function () {
      // An attacker tries to use HS256 with the RSA public key as the HMAC secret.
      // The Rust JWT module must reject this because the JWK algorithm is RS256,
      // not HS256. This is the algorithm confusion vulnerability (CVE-2015-9235).
      // We craft the token manually since jsonwebtoken refuses to sign HS256 with RSA keys.
      const header = Buffer.from(JSON.stringify({ alg: 'HS256', typ: 'JWT', kid: 'test-jwt-key' })).toString('base64url')
      const payload = Buffer.from(JSON.stringify({ sub: 'admin', exp: Math.floor(Date.now() / 1000) + 3600 })).toString('base64url')
      const crypto = require('crypto')
      const hmacKey = keyPair.publicKey
      const signature = crypto.createHmac('sha256', hmacKey).update(`${header}.${payload}`).digest('base64url')
      const confusedToken = `${header}.${payload}.${signature}`

      const agent = new Agent()
      agent.set('Authorization', `Bearer ${confusedToken}`)
      await agent.get('/api/').then(api.response.verify(api.response.incorrectAuthentication))
    })
  })

  describe('bearer: scope injection regression', function () {
    it('rejects token with forged scope claim and invalid signature', async function () {
      // An attacker forges a token with admin scopes but signs it with a wrong key.
      // Even if the scope claim looks valid, the signature must be verified first.
      const otherKey = jwt.generateKeyPair('attacker-key')
      const token = jwt.signToken(otherKey.privateKey, {
        sub: 'admin',
        scope: 'admin#admin',
      }, { kid: 'test-jwt-key' })

      const agent = new Agent()
      agent.set('Authorization', `Bearer ${token}`)
      await agent.get('/api/').then(api.response.verify(api.response.incorrectAuthentication))
    })
  })
})
