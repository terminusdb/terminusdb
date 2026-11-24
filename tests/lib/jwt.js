const crypto = require('crypto')
const jwt = require('jsonwebtoken')

/**
 * Generate an RSA key pair suitable for RS256 JWT signing and JWKS publishing.
 *
 * Returns an object with:
 *   - privateKey: PEM-encoded RSA private key (for signing tokens)
 *   - publicKey:  PEM-encoded RSA public key (for verification)
 *   - jwks:       JWKS JSON object with a single key entry
 *   - kid:        The key ID used in the JWKS and JWT header
 */
function generateKeyPair (kid) {
  if (!kid) {
    kid = 'test-key-' + crypto.randomBytes(4).toString('hex')
  }

  const { privateKey, publicKey } = crypto.generateKeyPairSync('rsa', {
    modulusLength: 2048,
    publicKeyEncoding: { type: 'spki', format: 'pem' },
    privateKeyEncoding: { type: 'pkcs8', format: 'pem' },
  })

  // Convert the public key to JWK format
  const pubKeyObj = crypto.createPublicKey(publicKey)
  const jwk = pubKeyObj.export({ format: 'jwk' })
  jwk.kid = kid
  jwk.alg = 'RS256'
  jwk.use = 'sig'

  const jwks = { keys: [jwk] }

  return { privateKey, publicKey, jwks, kid }
}

/**
 * Sign a JWT token with the given private key and payload.
 *
 * Options:
 *   - kid:        Key ID to put in the JWT header (should match JWKS)
 *   - algorithm:  Signing algorithm (default RS256)
 *   - expiresIn:  Token expiry (default '1h')
 *   - issuer:     Issuer claim
 *   - audience:   Audience claim
 */
function signToken (privateKey, payload, options) {
  options = options || {}
  const signOptions = {
    algorithm: options.algorithm || 'RS256',
    expiresIn: options.expiresIn || '1h',
  }
  if (options.issuer) signOptions.issuer = options.issuer
  if (options.audience) signOptions.audience = options.audience
  if (options.notBefore) signOptions.notBefore = options.notBefore

  const header = { kid: options.kid || 'test-key', typ: 'JWT' }
  return jwt.sign(payload, privateKey, { ...signOptions, header })
}

/**
 * Create a tiny HTTP server that serves a JWKS JSON response.
 *
 * Returns a Promise that resolves to { server, port, close }.
 */
function startJwksServer (jwks, preferredPort) {
  return new Promise((resolve, reject) => {
    const jwksJson = JSON.stringify(jwks)
    const server = require('http').createServer((req, res) => {
      if (req.url === '/jwks.json' || req.url === '/') {
        res.writeHead(200, { 'Content-Type': 'application/json' })
        res.end(jwksJson)
      } else {
        res.writeHead(404)
        res.end('Not found')
      }
    })

    server.on('error', reject)
    server.listen(preferredPort || 0, '127.0.0.1', () => {
      const port = server.address().port
      resolve({
        server,
        port,
        url: `http://127.0.0.1:${port}/jwks.json`,
        close: () => new Promise((resolve) => server.close(() => resolve())),
      })
    })
  })
}

module.exports = { generateKeyPair, signToken, startJwksServer }
