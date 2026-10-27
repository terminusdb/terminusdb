const { expect } = require('chai')
const { Agent, util, document } = require('../lib')

describe('log', function () {
  let agent
  let dbName

  before(function () {
    dbName = util.randomString()
    agent = new Agent({ dbName }).auth()
  })

  describe('logging with new db', function () {
    beforeEach(async function () {
      await agent.post(`/api/db/admin/${dbName}`).send({ label: 'Hello' })
    })

    afterEach(async function () {
      await agent.delete(`/api/db/admin/${dbName}`)
    })

    it('gets a log from changes', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0` }
      const result1 = await document.insert(agent, { instance: instance1 })

      const version1 = result1.headers['terminusdb-data-version'].split('branch:')[1]
      const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1` }

      const result2 = await document.insert(agent, { instance: instance2 })
      const version2 = result2.headers['terminusdb-data-version'].split('branch:')[1]

      const logRequest = await agent.get(`/api/log/admin/${dbName}`)
      const log = logRequest.body

      expect(log).to.have.lengthOf(3)
      expect(log[0].author).to.equal('default_author')
      expect(log[0].identifier).to.equal(version2)
      expect(log[1].identifier).to.equal(version1)
    })

    it('log entries include user field with authenticated user IRI', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id, a: 'xsd:string' }
      await document.insert(agent, { schema })
      const instance = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'val' }
      await document.insert(agent, { instance })

      const logRequest = await agent.get(`/api/log/admin/${dbName}`)
      const log = logRequest.body

      // Log entries are full commit documents returned by get_document.
      // The user field is the authenticated user's IRI stored as xsd:anyURI.
      expect(log.length).to.be.greaterThan(0)
      for (const entry of log) {
        expect(entry).to.have.property('user', 'terminusdb://system/data/User/admin')
        expect(entry).to.have.property('author')
        expect(entry).to.have.property('message')
        expect(entry).to.have.property('identifier')
        expect(entry).to.have.property('timestamp')
      }
    })

    it('gets a log from changes by commit', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0` }
      const result1 = await document.insert(agent, { instance: instance1 })

      const version1 = result1.headers['terminusdb-data-version'].split('branch:')[1]
      const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1` }

      const result2 = await document.insert(agent, { instance: instance2 })
      const version2 = result2.headers['terminusdb-data-version'].split('branch:')[1]

      const logRequest = await agent.get(`/api/log/admin/${dbName}/local/commit/${version2}`)
      const log = logRequest.body

      expect(log).to.have.lengthOf(3)
      expect(log[0].author).to.equal('default_author')
      expect(log[0].identifier).to.equal(version2)
      expect(log[1].identifier).to.equal(version1)
    })

    it('gets a log with count', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0` }
      const result1 = await document.insert(agent, { instance: instance1 })

      const version1 = result1.headers['terminusdb-data-version'].split('branch:')[1]
      const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1` }

      await document.insert(agent, { instance: instance2 })

      const logRequest = await agent.get(`/api/log/admin/${dbName}?start=1&count=1`)
      const log = logRequest.body

      expect(log).to.have.lengthOf(1)
      expect(log[0].author).to.equal('default_author')
      expect(log[0].identifier).to.equal(version1)
      // expect(log[1].identifier).to.equal(version1)
    })

    // ISO 8601 at nanosecond resolution, so bounds can sit exactly on a
    // commit timestamp (epoch seconds with sub-second fractions) rather
    // than a millisecond-rounded approximation of it.
    //
    // Timestamps are read from the raw JSON text: going through a JS
    // Number would emit the double's binary expansion (e.g. .857543017)
    // and land the bound a few nanoseconds off the stored xsd:decimal.
    const rawTimestamps = (bodyText) =>
      [...bodyText.matchAll(/"timestamp":\s*([0-9]+(?:\.[0-9]+)?)/g)].map((m) => m[1])
    const toIso = (rawTs) => {
      const [secs, frac = ''] = rawTs.split('.')
      const date = new Date(Number(secs) * 1000)
      return date.toISOString().replace(/\.\d{3}Z$/, `.${frac || '0'}Z`)
    }
    // Decimal-string <=: compare integer parts, then fraction digits.
    const tsLE = (a, b) => {
      const [ai, af = ''] = a.split('.')
      const [bi, bf = ''] = b.split('.')
      if (BigInt(ai) !== BigInt(bi)) return BigInt(ai) < BigInt(bi)
      const n = Math.max(af.length, bf.length)
      return BigInt((af + '0'.repeat(n)).slice(0, n) || '0') <=
        BigInt((bf + '0'.repeat(n)).slice(0, n) || '0')
    }
    const tsGE = (a, b) => a === b || !tsLE(a, b)

    it('starts the log at the newest commit at-or-before a startDateTime', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      for (let i = 0; i < 3; i++) {
        await document.insert(agent, { instance: { '@type': id, '@id': `terminusdb:///data/${id}/${i}` } })
      }

      const fullRes = await agent.get(`/api/log/admin/${dbName}`)
      const full = fullRes.body
      const rawTs = rawTimestamps(fullRes.text)
      const anchorRaw = rawTs[1]
      const anchorIso = toIso(anchorRaw)

      const logRequest = await agent.get(`/api/log/admin/${dbName}?startDateTime=${encodeURIComponent(anchorIso)}`)
      const log = logRequest.body

      const expected = full.find((c, i) => tsLE(rawTs[i], anchorRaw))
      expect(log[0].identifier).to.equal(expected.identifier)
      expect(log.length).to.equal(full.length - full.indexOf(expected))
    })

    it('starts the log at a startDateTime in a stream', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      for (let i = 0; i < 3; i++) {
        await document.insert(agent, { instance: { '@type': id, '@id': `terminusdb:///data/${id}/${i}` } })
      }

      const fullRes = await agent.get(`/api/log/admin/${dbName}`)
      const full = fullRes.body
      const rawTs = rawTimestamps(fullRes.text)
      const anchorRaw = rawTs[1]
      const anchorIso = toIso(anchorRaw)
      const expected = full.find((c, i) => tsLE(rawTs[i], anchorRaw))

      const res = await agent.get(`/api/log/admin/${dbName}?startDateTime=${encodeURIComponent(anchorIso)}&stream=true`)
      // The resolved offset tells a caller where the window's top commit sits
      // in newest-to-oldest order, so further pages can use plain `start`.
      expect(Number(res.headers['terminusdb-log-start'])).to.equal(full.indexOf(expected))
      const text = Buffer.isBuffer(res.body) ? res.body.toString('utf8') : res.text
      const lines = text.split('\n').filter((l) => l.trim()).map((l) => JSON.parse(l))
      expect(lines[0].identifier).to.equal(expected.identifier)
    })

    it('stops the log before commits older than an endDateTime', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      for (let i = 0; i < 3; i++) {
        await document.insert(agent, { instance: { '@type': id, '@id': `terminusdb:///data/${id}/${i}` } })
      }

      const fullRes = await agent.get(`/api/log/admin/${dbName}`)
      const full = fullRes.body
      const rawTs = rawTimestamps(fullRes.text)
      const boundRaw = rawTs[1]

      const logRequest = await agent.get(`/api/log/admin/${dbName}?endDateTime=${encodeURIComponent(toIso(boundRaw))}`)
      const log = logRequest.body

      const expectedCount = full.filter((c, i) => tsGE(rawTs[i], boundRaw)).length
      expect(log).to.have.lengthOf(expectedCount)
      const logRawTs = rawTimestamps(logRequest.text)
      for (const entry of logRawTs) {
        expect(tsGE(entry, boundRaw)).to.equal(true)
      }
    })

    it('rejects mutually exclusive log window parameters', async function () {
      // start and startDateTime both anchor the window's top edge and may
      // not be combined. endDateTime bounds the oldest edge and combines
      // with either.
      const iso = toIso(String(Date.now() / 1000))
      const response = await agent.get(`/api/log/admin/${dbName}?start=1&startDateTime=${encodeURIComponent(iso)}`)
      expect(response.status).to.equal(400)
      expect(response.body['api:error']['@type']).to.equal('api:MutuallyExclusiveLogParameters')
    })

    it('bounds the log between startDateTime and endDateTime', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      for (let i = 0; i < 5; i++) {
        await document.insert(agent, { instance: { '@type': id, '@id': `terminusdb:///data/${id}/${i}` } })
      }

      const fullRes = await agent.get(`/api/log/admin/${dbName}`)
      const full = fullRes.body
      const rawTs = rawTimestamps(fullRes.text)

      const topRaw = rawTs[1]
      const bottomRaw = rawTs[3]
      const params = `startDateTime=${encodeURIComponent(toIso(topRaw))}&endDateTime=${encodeURIComponent(toIso(bottomRaw))}`
      const logRequest = await agent.get(`/api/log/admin/${dbName}?${params}`)
      const log = logRequest.body

      const expected = full.filter((c, i) => tsLE(rawTs[i], topRaw) && tsGE(rawTs[i], bottomRaw))
      expect(log.map((c) => c.identifier)).to.deep.equal(expected.map((c) => c.identifier))
    })

    it('bounds the log between an offset start and endDateTime', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      for (let i = 0; i < 5; i++) {
        await document.insert(agent, { instance: { '@type': id, '@id': `terminusdb:///data/${id}/${i}` } })
      }

      const fullRes = await agent.get(`/api/log/admin/${dbName}`)
      const full = fullRes.body
      const rawTs = rawTimestamps(fullRes.text)

      const bottomRaw = rawTs[3]
      const logRequest = await agent.get(`/api/log/admin/${dbName}?start=1&endDateTime=${encodeURIComponent(toIso(bottomRaw))}`)
      const log = logRequest.body

      const expected = full.filter((c, i) => i >= 1 && tsGE(rawTs[i], bottomRaw))
      expect(log.map((c) => c.identifier)).to.deep.equal(expected.map((c) => c.identifier))
    })

    it('rejects a malformed datetime parameter', async function () {
      const response = await agent.get(`/api/log/admin/${dbName}?startDateTime=not-a-date`)
      expect(response.status).to.equal(400)
      expect(response.body['api:error']['@type']).to.equal('api:InvalidLogDateTime')
    })
  })

  it('gets an error from _system', async function () {
    const response = await agent.get('/api/log/_system')

    expect(response.status).to.equal(400)
    expect(response.body['api:error']['@type']).to.equal('api:ResourceHasNoHistory')
  })

  it('gets an error from somedb/_meta', async function () {
    const response = await agent.get('/api/log/someorg/somedb/_meta')

    expect(response.status).to.equal(400)
    expect(response.body['api:error']['@type']).to.equal('api:ResourceHasNoHistory')
  })

  it('gets an error from somedb/local/_commits', async function () {
    const response = await agent.get('/api/log/someorg/somedb/local/_commits')

    expect(response.status).to.equal(400)
    expect(response.body['api:error']['@type']).to.equal('api:ResourceHasNoHistory')
  })
})
