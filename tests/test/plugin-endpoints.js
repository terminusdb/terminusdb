const { expect } = require('chai')
const net = require('net')
const { Agent, db, document, util } = require('../lib')

describe('plugin-endpoints', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('vectorlink plugin routes', function () {
    it('GET /api/search/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .get(`/api/search/${agent.orgName}/${agent.dbName}`)
        .query({ query: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:VectorlinkEndpointNotConfigured')
    })

    it('POST /api/search/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .post(`/api/search/${agent.orgName}/${agent.dbName}`)
        .send({ query: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:VectorlinkEndpointNotConfigured')
    })

    it('POST /api/similar/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .post(`/api/similar/${agent.orgName}/${agent.dbName}`)
        .send({ id: 'test', query: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:VectorlinkEndpointNotConfigured')
    })

    it('GET /api/duplicates/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .get(`/api/duplicates/${agent.orgName}/${agent.dbName}`)
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:VectorlinkEndpointNotConfigured')
    })

    it('POST /api/plugin/search-resolve/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .post(`/api/plugin/search-resolve/${agent.orgName}/${agent.dbName}`)
        .send({ source: 'test', target: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:VectorlinkEndpointNotConfigured')
    })

    it('GET /api/index/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .get(`/api/index/${agent.orgName}/${agent.dbName}`)
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type').that.matches(/api:(Vectorlink|SemanticIndexer)EndpointNotConfigured/)
    })

    it('POST /api/compare returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .post('/api/compare')
        .send({ method: 'cosine', source: 'test', target: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type').that.matches(/api:(Vectorlink|SemanticIndexer)EndpointNotConfigured/)
    })
  })

  describe('index endpoint response format', function () {
    it('GET /api/index/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .get(`/api/index/${agent.orgName}/${agent.dbName}`)
        .query({ commit_id: 'placeholder' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type').that.matches(/api:(Vectorlink|SemanticIndexer)EndpointNotConfigured/)
    })

    it('POST /api/index/{path} returns valid JSON (not Prolog json{} term)', async function () {
      const r = await agent
        .post(`/api/index/${agent.orgName}/${agent.dbName}/local/branch/main`)
      // Regression: handler used write(json{...}) which produced invalid JSON
      // like "json{@type:api:IndexResponse,api:status:api:success}" instead of
      // proper JSON. The response must be parseable JSON in all cases
      // (success, error, or endpoint-not-configured).
      expect(r.text).to.not.match(/^json\{/)
      expect(r.body).to.be.an('object')
      expect(r.body).to.have.property('@type')
      expect(r.body).to.have.property('api:status')
    })

    it('DELETE /api/index/{path} returns valid JSON (not Prolog json{} term)', async function () {
      const r = await agent
        .delete(`/api/index/${agent.orgName}/${agent.dbName}/local/branch/main`)
      // Regression: handler used write(json{...}) which produced invalid JSON.
      // The response must be parseable JSON in all cases.
      expect(r.text).to.not.match(/^json\{/)
      expect(r.body).to.be.an('object')
      expect(r.body).to.have.property('@type')
      expect(r.body).to.have.property('api:status')
    })
  })

  describe('compress_ids parameter validation', function () {
    // Detect whether vectorlink endpoint is configured by probing a search request
    let endpointConfigured = false

    before(async function () {
      const probe = await agent
        .get(`/api/search/${agent.orgName}/${agent.dbName}`)
        .query({ query: 'test' })
      endpointConfigured = probe.body['api:error'] &&
        probe.body['api:error']['@type'] !== 'api:VectorlinkEndpointNotConfigured'
    })

    it('GET /api/search accepts compress_ids=true without parameter error', async function () {
      const r = await agent
        .get(`/api/search/${agent.orgName}/${agent.dbName}`)
        .query({ query: 'test', compress_ids: 'true' })
      // Should not get a BadParameterType error for compress_ids
      const errType = r.body['api:error'] && r.body['api:error']['@type']
      if (endpointConfigured) {
        expect(errType).to.not.equal('api:BadParameterType')
      } else {
        expect(r.status).to.equal(400)
        expect(errType).to.equal('api:VectorlinkEndpointNotConfigured')
      }
    })

    it('GET /api/search accepts compress_ids=false without parameter error', async function () {
      const r = await agent
        .get(`/api/search/${agent.orgName}/${agent.dbName}`)
        .query({ query: 'test', compress_ids: 'false' })
      const errType = r.body['api:error'] && r.body['api:error']['@type']
      if (endpointConfigured) {
        expect(errType).to.not.equal('api:BadParameterType')
      } else {
        expect(r.status).to.equal(400)
        expect(errType).to.equal('api:VectorlinkEndpointNotConfigured')
      }
    })

    it('GET /api/search rejects invalid compress_ids with 400 BadParameterType when endpoint is configured', async function () {
      const r = await agent
        .get(`/api/search/${agent.orgName}/${agent.dbName}`)
        .query({ query: 'test', compress_ids: 'notabool' })
      expect(r.status).to.equal(400)
      if (endpointConfigured) {
        expect(r.body['api:error']).to.have.property('@type', 'api:BadParameterType')
      } else {
        // compress_flag is called after endpoint check, so invalid value
        // is only rejected when endpoint is configured
        expect(r.body['api:error']).to.have.property('@type', 'api:VectorlinkEndpointNotConfigured')
      }
    })

    it('POST /api/similar accepts compress_ids=true without parameter error', async function () {
      const r = await agent
        .post(`/api/similar/${agent.orgName}/${agent.dbName}`)
        .query({ compress_ids: 'true' })
        .send({ id: 'test', query: 'test' })
      if (endpointConfigured) {
        // May get 404 or other non-parameter error, but not BadParameterType for compress_ids
        const errType = r.body['api:error'] && r.body['api:error']['@type']
        expect(errType).to.not.equal('api:BadParameterType')
      } else {
        expect(r.status).to.equal(400)
        expect(r.body['api:error']).to.have.property('@type', 'api:VectorlinkEndpointNotConfigured')
      }
    })

    it('POST /api/similar rejects invalid compress_ids with 400 BadParameterType when endpoint is configured', async function () {
      const r = await agent
        .post(`/api/similar/${agent.orgName}/${agent.dbName}`)
        .query({ compress_ids: 'notabool' })
        .send({ id: 'test', query: 'test' })
      expect(r.status).to.equal(400)
      if (endpointConfigured) {
        expect(r.body['api:error']).to.have.property('@type', 'api:BadParameterType')
      } else {
        expect(r.body['api:error']).to.have.property('@type', 'api:VectorlinkEndpointNotConfigured')
      }
    })

    it('GET /api/duplicates accepts compress_ids=true without parameter error', async function () {
      const r = await agent
        .get(`/api/duplicates/${agent.orgName}/${agent.dbName}`)
        .query({ compress_ids: 'true' })
      const errType = r.body['api:error'] && r.body['api:error']['@type']
      if (endpointConfigured) {
        expect(errType).to.not.equal('api:BadParameterType')
      } else {
        expect(r.status).to.equal(400)
        expect(errType).to.equal('api:VectorlinkEndpointNotConfigured')
      }
    })

    it('GET /api/duplicates rejects invalid compress_ids with 400 BadParameterType when endpoint is configured', async function () {
      const r = await agent
        .get(`/api/duplicates/${agent.orgName}/${agent.dbName}`)
        .query({ compress_ids: 'notabool' })
      expect(r.status).to.equal(400)
      if (endpointConfigured) {
        expect(r.body['api:error']).to.have.property('@type', 'api:BadParameterType')
      } else {
        expect(r.body['api:error']).to.have.property('@type', 'api:VectorlinkEndpointNotConfigured')
      }
    })
  })

  describe('events endpoint', function () {
    function eventsUrl (path, query) {
      let url = `${agent.baseUrl}/api/events/${path}`
      if (query) { url += `?${query}` }
      return url
    }

    function authHeaders () {
      return { Authorization: util.authorizationHeader(agent) }
    }

    // Read NDJSON lines until predicate matches or the timeout fires.
    // Returns the parsed line, or null on timeout.
    async function waitForNDJSON (body, predicate, timeoutMs) {
      const reader = body.getReader()
      const decoder = new TextDecoder()
      let buf = ''
      const deadline = Date.now() + timeoutMs
      try {
        while (Date.now() < deadline) {
          const remaining = deadline - Date.now()
          const chunk = await Promise.race([
            reader.read(),
            new Promise((_resolve, reject) => setTimeout(() => reject(new Error('timeout')), remaining)),
          ])
          if (chunk.done) return null
          buf += decoder.decode(chunk.value, { stream: true })
          const lines = buf.split('\n')
          buf = lines.pop()
          for (const line of lines) {
            if (!line.trim()) continue
            const parsed = JSON.parse(line)
            const match = predicate(parsed, line)
            if (match) return match === true ? parsed : match
          }
        }
        return null
      } catch (e) {
        if (e.message === 'timeout') return null
        throw e
      } finally {
        try { reader.releaseLock() } catch { /* ignore */ }
      }
    }

    it('GET /api/events without a scope returns 404', async function () {
      const r = await fetch(`${agent.baseUrl}/api/events`, { headers: authHeaders() })
      expect(r.status).to.equal(404)
    })

    it('GET /api/events/{org} returns 404 — a bare organization is not a scope', async function () {
      const r = await fetch(eventsUrl(agent.orgName), { headers: authHeaders() })
      expect(r.status).to.equal(404)
    })

    it('GET /api/events/{org}/{db}/_meta returns 400 — meta graph is not a scope', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}/_meta`), { headers: authHeaders() })
      expect(r.status).to.equal(400)
    })

    it('rejects unauthenticated requests', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`))
      expect(r.status).to.equal(401)
    })

    it('rejects unknown kind values', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'kind=diff'), { headers: authHeaders() })
      expect(r.status).to.equal(400)
    })

    it('accepts doc, json and any kinds on a named channel', async function () {
      for (const kind of ['doc', 'json', 'any']) {
        const controller = new AbortController()
        const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, `channel=michan&kind=${kind}`), {
          headers: authHeaders(),
          signal: controller.signal,
        })
        expect(r.status).to.equal(200)
        controller.abort()
      }
    })

    it('rejects a kind the channel cannot carry', async function () {
      // id events exist only on the reserved @commit channel
      const r1 = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=michan&kind=id'), { headers: authHeaders() })
      expect(r1.status).to.equal(400)
      // @commit channel carries only id events
      const r2 = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=@commit&kind=doc'), { headers: authHeaders() })
      expect(r2.status).to.equal(400)
    })

    it('rejects unknown @-prefixed channel keys', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=@bogus'), { headers: authHeaders() })
      expect(r.status).to.equal(400)
    })

    it('rejects since values other than now', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'since=abc123'), { headers: authHeaders() })
      expect(r.status).to.equal(400)
    })

    it('rejects channel keys with invalid characters', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=bad%20key'), { headers: authHeaders() })
      expect(r.status).to.equal(400)
    })

    it('rejects MQTT wildcard levels in channel keys', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=a%2F%2B%2Fc'), { headers: authHeaders() })
      expect(r.status).to.equal(400)
    })

    it('rejects channel keys longer than 256 characters', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, `channel=${'a'.repeat(257)}`), { headers: authHeaders() })
      expect(r.status).to.equal(400)
    })

    it('database scope delivers an id envelope for a commit', async function () {
      this.timeout(30000)
      const controller = new AbortController()
      const response = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`), {
        headers: authHeaders(),
        signal: controller.signal,
      })
      expect(response.status).to.equal(200)
      expect(response.headers.get('Content-Type')).to.contain('application/x-ndjson')

      const eventPromise = waitForNDJSON(response.body, (e) => e.event === 'id', 15000)
      await document.insert(agent, {
        schema: { '@id': 'EvtDoc', '@type': 'Class', name: 'xsd:string' },
      })
      const event = await eventPromise
      controller.abort()

      expect(event).to.not.equal(null)
      expect(event.channel).to.contain(`${agent.orgName}/${agent.dbName}/local/branch/`)
      expect(event.data).to.have.property('commit')
    })

    it('a named channel does not receive commit events', async function () {
      this.timeout(30000)
      const controller = new AbortController()
      const response = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=michan'), {
        headers: authHeaders(),
        signal: controller.signal,
      })
      expect(response.status).to.equal(200)

      const eventPromise = waitForNDJSON(response.body, () => true, 5000)
      await document.insert(agent, {
        instance: { '@type': 'EvtDoc', name: 'not-on-michan' },
      })
      const event = await eventPromise
      controller.abort()

      expect(event).to.equal(null)
    })

    it('idle timeout closes the stream', async function () {
      this.timeout(10000)
      const response = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'timeout=1'), { headers: authHeaders() })
      expect(response.status).to.equal(200)
      // Server ends the body after ~1s of no events — no abort needed.
      const reader = response.body.getReader()
      while (!(await reader.read()).done) { /* drain */ }
    })

    // Raw-socket upgrade helper: sends the GET with Upgrade: tdb-events,
    // resolves { socket, head, body } once the 101 headers are complete;
    // body accumulates every byte received after the header terminator.
    function upgradeEvents (scopePath, query) {
      const port = new URL(agent.baseUrl).port
      const auth = util.authorizationHeader(agent)
      const socket = net.connect(Number(port), '127.0.0.1')
      let raw = Buffer.alloc(0)
      return new Promise((resolve, reject) => {
        socket.on('error', reject)
        socket.on('data', (chunk) => {
          raw = Buffer.concat([raw, chunk])
          const split = raw.indexOf('\r\n\r\n')
          if (split < 0) { return }
          socket.removeAllListeners('data')
          const head = raw.subarray(0, split).toString()
          const body = { buf: raw.subarray(split + 4) }
          socket.on('data', (c) => { body.buf = Buffer.concat([body.buf, c]) })
          resolve({ socket, head, body })
        })
        socket.on('connect', () => {
          socket.write(
            `GET /api/events/${scopePath}${query ? `?${query}` : ''} HTTP/1.1\r\n` +
            `Host: 127.0.0.1:${port}\r\nAuthorization: ${auth}\r\n` +
            'Upgrade: tdb-events\r\nConnection: upgrade\r\n\r\n',
          )
        })
      })
    }

    it('Upgrade: tdb-events switches to a raw stream that delivers events', async function () {
      // The raw-socket upgrade is enterprise-only (the socket forwarder
      // lives in the enterprise patch); community answers with a
      // chunked 200 instead of 101.
      if (!util.enterprise) this.skip()
      this.timeout(30000)
      const { socket, head, body } = await upgradeEvents(
        `${agent.orgName}/${agent.dbName}`, 'timeout=20')
      try {
        expect(head).to.contain('101')
        expect(head).to.contain('upgrade: tdb-events')

        const eventPromise = new Promise((resolve) => {
          const check = () => {
            const line = body.buf.toString().split('\n').find((l) => l.trim().startsWith('{'))
            if (line) { resolve(JSON.parse(line)) } else { setImmediate(check) }
          }
          check()
        })
        await document.insert(agent, {
          instance: { '@type': 'EvtDoc', name: 'upgraded' },
        })
        const event = await eventPromise
        expect(event.event).to.equal('id')
      } finally {
        socket.destroy()
      }
    })

    it('branch scope delivers commit events for that branch', async function () {
      this.timeout(30000)
      const controller = new AbortController()
      const branchPath = `${agent.orgName}/${agent.dbName}/local/branch/main`
      const response = await fetch(eventsUrl(branchPath), {
        headers: authHeaders(),
        signal: controller.signal,
      })
      expect(response.status).to.equal(200)

      const eventPromise = waitForNDJSON(response.body, (e) => e.event === 'id', 15000)
      await document.insert(agent, {
        instance: { '@type': 'EvtDoc', name: 'branch-scope' },
      })
      const event = await eventPromise
      controller.abort()

      expect(event).to.not.equal(null)
      expect(event.channel).to.equal(branchPath)
    })

    it('explicit channel=@commit subscribes to the reserved commit channel', async function () {
      this.timeout(30000)
      const controller = new AbortController()
      const response = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=@commit'), {
        headers: authHeaders(),
        signal: controller.signal,
      })
      expect(response.status).to.equal(200)

      const eventPromise = waitForNDJSON(response.body, (e) => e.event === 'id', 15000)
      await document.insert(agent, {
        instance: { '@type': 'EvtDoc', name: 'explicit-commit' },
      })
      const event = await eventPromise
      controller.abort()

      expect(event).to.not.equal(null)
    })

    it('POST without a channel parameter returns 400', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: JSON.stringify({ '@type': 'EvtDoc', name: 'x' }),
      })
      expect(r.status).to.equal(400)
    })

    it('POST to the reserved @commit channel returns 400', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=@commit'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: JSON.stringify({ '@type': 'EvtDoc', name: 'x' }),
      })
      expect(r.status).to.equal(400)
    })

    it('POST to a channel named commit succeeds — only @-keys are reserved', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=commit'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: '{"regular":"channel"}',
      })
      expect(r.status).to.equal(200)
    })

    it('POST rejects unauthenticated requests', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=michan'), {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ '@type': 'EvtDoc', name: 'x' }),
      })
      expect(r.status).to.equal(401)
    })

    it('POST rejects a message that fails schema check', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=michan&kind=doc'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: JSON.stringify({ '@type': 'EvtDoc', unknown_field: 'x' }),
      })
      expect(r.status).to.equal(400)
    })

    it('GET to a database that does not exist returns 404', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/nodb_xyz`), {
        headers: authHeaders(),
      })
      expect(r.status).to.equal(404)
    })

    it('POST rejects malformed JSON', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=michan'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: '{"@type":"EvtDoc",',
      })
      expect(r.status).to.equal(400)
    })

    it('POST rejects JSON that is not an object', async function () {
      for (const body of ['[1,2,3]', '"hello"', '42']) {
        const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=michan&kind=doc'), {
          method: 'POST',
          headers: { ...authHeaders(), 'Content-Type': 'application/json' },
          body,
        })
        expect(r.status).to.equal(400)
      }
    })

    it('POST kind=json publishes unchecked raw JSON', async function () {
      for (const body of ['[1,2,3]', '"hello"', '{"@type":"NoSuchClass","x":1}']) {
        const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=michan&kind=json'), {
          method: 'POST',
          headers: { ...authHeaders(), 'Content-Type': 'application/json' },
          body,
        })
        expect(r.status).to.equal(200)
      }
    })

    it('POST rejects kind values other than doc and json', async function () {
      for (const kind of ['id', 'any', 'diff']) {
        const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, `channel=michan&kind=${kind}`), {
          method: 'POST',
          headers: { ...authHeaders(), 'Content-Type': 'application/json' },
          body: JSON.stringify({ '@type': 'EvtDoc', name: 'x' }),
        })
        expect(r.status).to.equal(400)
      }
    })

    it('POST kind=doc is the explicit schema-bound form', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=michan&kind=doc'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: JSON.stringify({ '@type': 'EvtDoc', name: 'x' }),
      })
      expect(r.status).to.equal(200)
    })

    it('POST kind=json still rejects malformed JSON', async function () {
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=michan&kind=json'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: '{"a":1,',
      })
      expect(r.status).to.equal(400)
    })

    it('POST kind=json round-trips high-precision numbers verbatim', async function () {
      this.timeout(30000)
      const controller = new AbortController()
      const response = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=prec&kind=json'), {
        headers: authHeaders(),
        signal: controller.signal,
      })
      expect(response.status).to.equal(200)

      // Capture the raw line — JSON.parse would truncate to f64.
      const eventPromise = waitForNDJSON(
        response.body,
        (e, raw) => e.event === 'message' && raw,
        15000)
      const digits = '123456789012345678901234567890.123456789'
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=prec&kind=json'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: `{"amount": ${digits}}`,
      })
      expect(r.status).to.equal(200)
      const raw = await eventPromise
      controller.abort()

      expect(raw).to.be.a('string')
      expect(raw).to.contain(digits)
    })

    it('published message reaches subscribers of that channel', async function () {
      this.timeout(30000)
      const controller = new AbortController()
      const response = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=notify'), {
        headers: authHeaders(),
        signal: controller.signal,
      })
      expect(response.status).to.equal(200)

      const eventPromise = waitForNDJSON(response.body, (e) => e.event === 'message', 15000)
      const r = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=notify'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: JSON.stringify({ '@type': 'EvtDoc', name: 'hello' }),
      })
      expect(r.status).to.equal(200)
      const event = await eventPromise
      controller.abort()

      expect(event).to.not.equal(null)
      expect(event.channel).to.equal(`${agent.orgName}/${agent.dbName}`)
      expect(event.data).to.have.property('topic', 'notify')
      expect(event.data).to.have.property('kind', 'json')
      expect(event.data.message).to.have.property('name', 'hello')
    })

    it('kind=json subscriber receives raw JSON but not doc messages', async function () {
      this.timeout(30000)
      const controller = new AbortController()
      const response = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=split&kind=json'), {
        headers: authHeaders(),
        signal: controller.signal,
      })
      expect(response.status).to.equal(200)

      // A doc publish on the same channel must not reach a json subscriber.
      const docEvent = waitForNDJSON(response.body, () => true, 5000)
      const dr = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=split&kind=doc'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: JSON.stringify({ '@type': 'EvtDoc', name: 'doc-msg' }),
      })
      expect(dr.status).to.equal(200)
      expect(await docEvent).to.equal(null)

      const jsonEvent = waitForNDJSON(response.body, (e) => e.event === 'message', 15000)
      const jr = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=split&kind=json'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: '{"raw":true}',
      })
      expect(jr.status).to.equal(200)
      const event = await jsonEvent
      controller.abort()

      expect(event).to.not.equal(null)
      expect(event.data).to.have.property('kind', 'json')
      expect(event.data.message).to.have.property('raw', true)
    })

    it('kind=any subscriber receives both doc and json messages', async function () {
      this.timeout(30000)
      const controller = new AbortController()
      const response = await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=both&kind=any'), {
        headers: authHeaders(),
        signal: controller.signal,
      })
      expect(response.status).to.equal(200)

      const kinds = new Set()
      const collect = async () => {
        for (let i = 0; i < 2; i++) {
          const e = await waitForNDJSON(response.body, (ev) => ev.event === 'message', 15000)
          if (e === null) break
          kinds.add(e.data.kind)
        }
      }
      const collecting = collect()

      await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=both&kind=doc'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: JSON.stringify({ '@type': 'EvtDoc', name: 'doc-msg' }),
      })
      await fetch(eventsUrl(`${agent.orgName}/${agent.dbName}`, 'channel=both&kind=json'), {
        method: 'POST',
        headers: { ...authHeaders(), 'Content-Type': 'application/json' },
        body: '{"raw":true}',
      })
      await collecting
      controller.abort()

      expect([...kinds]).to.have.members(['doc', 'json'])
    })
  })
})
