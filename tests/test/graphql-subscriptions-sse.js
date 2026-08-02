const { expect } = require('chai')
const { Agent, db, document, optimize } = require('../lib')

function cancelBody (res) {
  try { res?._abortController?.abort?.() } catch { /* already closed */ }
  try { res?.body?.cancel?.() } catch { /* already closed */ }
}

function parseSSEBlock (eventBlock) {
  if (eventBlock.startsWith(':')) return null
  let eventType = 'message'
  let dataLine = null
  for (const line of eventBlock.split('\n')) {
    if (line.startsWith('event: ')) {
      eventType = line.slice(7).trim()
    } else if (line.startsWith('data: ')) {
      dataLine = line.slice(6)
    } else if (line.startsWith('data:')) {
      dataLine = line.slice(5)
    }
  }
  if (dataLine === null) return null
  try {
    const parsed = JSON.parse(dataLine)
    if (parsed === null || typeof parsed !== 'object') {
      return { _eventType: eventType, data: parsed }
    }
    parsed._eventType = eventType
    return parsed
  } catch { /* skip unparseable */ }
  return null
}

function parseSSEStream (body) {
  const reader = body.getReader()
  const decoder = new TextDecoder()
  let buffer = ''
  const queue = []
  const state = { done: false }
  let waitResolve
  const pump = async () => {
    while (!state.done) {
      const { done: rdone, value } = await reader.read()
      if (rdone) { state.done = true; break }
      buffer += decoder.decode(value, { stream: true })
      const events = buffer.split('\n\n')
      buffer = events.pop()
      for (const eventBlock of events) {
        const parsed = parseSSEBlock(eventBlock)
        if (parsed) queue.push(parsed)
      }
      if (waitResolve) { waitResolve(); waitResolve = null }
    }
    if (waitResolve) { waitResolve(); waitResolve = null }
  }
  pump()
  return {
    async next () {
      while (queue.length === 0 && !state.done) {
        await new Promise((resolve) => { waitResolve = resolve })
      }
      return queue.length > 0 ? queue.shift() : null
    },
    cancel () {
      state.done = true
      try { reader.cancel() } catch { /* already closed */ }
      try { reader.releaseLock() } catch { /* already released */ }
      if (waitResolve) { waitResolve(); waitResolve = null }
    },
  }
}

async function waitForSSEEvent (body, predicate, timeoutMs) {
  const parser = parseSSEStream(body)
  const timeout = new Promise((_resolve, reject) =>
    setTimeout(() => { parser.cancel(); reject(new Error('SSE timeout waiting for matching event')) }, timeoutMs || 5000))
  const search = (async () => {
    while (true) {
      const event = await parser.next()
      if (event === null) throw new Error('SSE stream ended without a matching event')
      if (predicate(event)) { parser.cancel(); return event }
    }
  })()
  return Promise.race([search, timeout])
}

async function waitForEventByName (parser, name, timeoutMs) {
  const startTime = Date.now()
  while (Date.now() - startTime < (timeoutMs || 10000)) {
    const event = await parser.next()
    if (event === null) break
    if (event?.data?.Product_added?.name === name) return event
  }
  return null
}

describe('GraphQL subscriptions over SSE', function () {
  this.timeout(15000)
  let agent
  let branchPath
  let sseUrl

  // Product type with name and description fields.
  // Subscriptions request only a subset of fields (e.g. _id name) to
  // verify that selection set filtering excludes unrequested fields
  // (e.g. description must NOT appear in the event payload).
  const schema = [{
    '@type': '@context',
    '@base': 'terminusdb:///data/',
    '@schema': 'terminusdb:///schema#',
  }, {
    '@id': 'Product',
    '@type': 'Class',
    '@key': { '@type': 'Lexical', '@fields': ['name'] },
    name: 'xsd:string',
    description: 'xsd:string',
  }]

  before(async function () {
    this.timeout(15000)
    agent = new Agent()
    agent.auth()
    await db.create(agent, { label: 'GraphQL SSE Subscriptions Test', schema: true })
    await document.insert(agent, { schema, fullReplace: true })
    // Squash the commit graph after schema insert for deterministic performance.
    const dbPath = `${agent.orgName}/${agent.dbName}`
    await optimize.optimizeDatabase(agent, dbPath, 'main')
    branchPath = agent.orgName + '/' + agent.dbName + '/local/branch/main'
    sseUrl = agent.baseUrl + '/api/graphql/' + branchPath
  })

  after(async function () {
    this.timeout(30000)
    await db.delete(agent)
  })

  async function subscribeSSE (query) {
    const controller = new AbortController()
    const auth = 'Basic ' + Buffer.from(agent.user + ':' + agent.password).toString('base64')
    const res = await fetch(sseUrl, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        Accept: 'text/event-stream',
        Authorization: auth,
      },
      body: JSON.stringify({ query }),
      signal: controller.signal,
    })
    res._abortController = controller
    return res
  }

  describe('authentication', function () {
    it('should reject without Authorization header', async function () {
      const res = await fetch(sseUrl, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Accept: 'text/event-stream',
        },
        body: JSON.stringify({ query: 'subscription { Product_added { _id } }' }),
      })
      expect(res.status).to.equal(401)
    })

    it('should accept with Authorization header and return text/event-stream', async function () {
      const res = await subscribeSSE('subscription { Product_added { _id } }')
      expect(res.status).to.equal(200)
      expect(res.headers.get('content-type')).to.include('text/event-stream')
      // Abort the stream since we're just testing headers here.
      cancelBody(res)
    })
  })

  describe('subscribe and receive _added events', function () {
    it('should receive only _id and name (not description) on insert', async function () {
      const res = await subscribeSSE('subscription { Product_added { _id name } }')
      const eventPromise = waitForSSEEvent(res.body,
        (e) => e.data?.Product_added, 5000)

      await document.insert(agent, {
        instance: [{
          '@type': 'Product',
          name: 'Widget',
          description: 'A widget product',
        }],
      })

      const event = await eventPromise
      expect(event.data.Product_added).to.exist
      expect(event.data.Product_added._id).to.exist
      expect(event.data.Product_added.name).to.equal('Widget')
      // CRITICAL: exactly 2 keys — selection set filtering must be exact.
      const keys = Object.keys(event.data.Product_added)
      expect(keys.length).to.equal(2, `expected exactly 2 keys (_id, name), got ${keys.length}: ${JSON.stringify(keys)}`)
      expect(keys).to.include('_id')
      expect(keys).to.include('name')
      cancelBody(res)
    })
  })

  describe('subscribe and receive _changed events', function () {
    it('should receive only _id and name (not description) on replace', async function () {
      // Insert a document to change later.
      await document.insert(agent, {
        instance: [{
          '@type': 'Product',
          name: 'ChangeMe',
          description: 'Original description',
        }],
      })
      const getResult = await document.get(agent, { query: { type: 'Product', as_list: true } })
      const insertedDoc = getResult.body.find((d) => d.name === 'ChangeMe')
      expect(insertedDoc).to.exist

      const res = await subscribeSSE('subscription { Product_changed { _id name } }')
      const eventPromise = waitForSSEEvent(res.body,
        (e) => e.data?.Product_changed, 5000)

      // Replace with updated description (subscription only requests _id and name).
      const updatedDoc = { ...insertedDoc, description: 'Updated description' }
      await document.replace(agent, { instance: updatedDoc })

      const event = await eventPromise
      expect(event.data.Product_changed).to.exist
      expect(event.data.Product_changed._id).to.exist
      expect(event.data.Product_changed.name).to.equal('ChangeMe')
      // CRITICAL: exactly 2 keys — selection set filtering must be exact.
      const keys = Object.keys(event.data.Product_changed)
      expect(keys.length).to.equal(2, `expected exactly 2 keys (_id, name), got ${keys.length}: ${JSON.stringify(keys)}`)
      expect(keys).to.include('_id')
      expect(keys).to.include('name')
      cancelBody(res)
    })
  })

  describe('subscribe and receive _deleted events', function () {
    it('should receive _id on delete', async function () {
      await document.insert(agent, {
        instance: [{
          '@type': 'Product',
          name: 'DeleteMe',
          description: 'To be deleted',
        }],
      })
      const getResult = await document.get(agent, { query: { type: 'Product', as_list: true } })
      const insertedDoc = getResult.body.find((d) => d.name === 'DeleteMe')
      const docId = insertedDoc['@id']

      const res = await subscribeSSE('subscription { Product_deleted { _id } }')
      const eventPromise = waitForSSEEvent(res.body,
        (e) => e.data?.Product_deleted, 5000)

      await document.delete(agent, { query: { id: docId } })

      const event = await eventPromise
      // Deleted events resolve the last document from pre-commit state.
      // With selection set { _id }, only _id is returned.
      expect(event.data.Product_deleted).to.exist
      expect(event.data.Product_deleted._id).to.exist
      // CRITICAL: exactly 1 key — selection set filtering must be exact.
      const keys = Object.keys(event.data.Product_deleted)
      expect(keys.length).to.equal(1, `expected exactly 1 key (_id), got ${keys.length}: ${JSON.stringify(keys)}`)
      expect(keys).to.include('_id')
      cancelBody(res)
    })

    it('should resolve last document fields on delete with _id and name', async function () {
      await document.insert(agent, {
        instance: [{
          '@type': 'Product',
          name: 'DeleteMeWithFields',
          description: 'To be deleted with field check',
        }],
      })
      const getResult = await document.get(agent, { query: { type: 'Product', as_list: true } })
      const insertedDoc = getResult.body.find((d) => d.name === 'DeleteMeWithFields')
      const docId = insertedDoc['@id']

      const res = await subscribeSSE('subscription { Product_deleted { _id name } }')
      const eventPromise = waitForSSEEvent(res.body,
        (e) => e.data?.Product_deleted, 5000)

      await document.delete(agent, { query: { id: docId } })

      const event = await eventPromise
      // Deleted events resolve the last document from pre-commit state.
      // With { _id name } selection, both fields are present.
      expect(event.data.Product_deleted._id).to.exist
      expect(event.data.Product_deleted.name).to.equal('DeleteMeWithFields')
      // CRITICAL: exactly 2 keys — selection set filtering must be exact.
      const keys = Object.keys(event.data.Product_deleted)
      expect(keys.length).to.equal(2, `expected exactly 2 keys (_id, name), got ${keys.length}: ${JSON.stringify(keys)}`)
      expect(keys).to.include('_id')
      expect(keys).to.include('name')
      cancelBody(res)
    })
  })

  describe('selection set filtering', function () {
    it('should return only _id when only _id is requested', async function () {
      const res = await subscribeSSE('subscription { Product_added { _id } }')
      const eventPromise = waitForSSEEvent(res.body,
        (e) => e.data?.Product_added, 5000)

      await document.insert(agent, {
        instance: [{
          '@type': 'Product',
          name: 'IdOnly',
          description: 'Should not appear',
        }],
      })

      const event = await eventPromise
      expect(event.data.Product_added).to.exist
      expect(event.data.Product_added._id).to.exist
      // CRITICAL: exactly 1 key — selection set filtering must be exact.
      const keys = Object.keys(event.data.Product_added)
      expect(keys.length).to.equal(1, `expected exactly 1 key (_id), got ${keys.length}: ${JSON.stringify(keys)}`)
      expect(keys).to.include('_id')
      cancelBody(res)
    })

    it('should return only _id and name when both are requested', async function () {
      const res = await subscribeSSE('subscription { Product_added { _id name } }')
      const eventPromise = waitForSSEEvent(res.body,
        (e) => e.data?.Product_added, 5000)

      await document.insert(agent, {
        instance: [{
          '@type': 'Product',
          name: 'IdAndName',
          description: 'Should not appear',
        }],
      })

      const event = await eventPromise
      expect(event.data.Product_added._id).to.exist
      expect(event.data.Product_added.name).to.equal('IdAndName')
      // CRITICAL: exactly 2 keys — selection set filtering must be exact.
      const keys = Object.keys(event.data.Product_added)
      expect(keys.length).to.equal(2, `expected exactly 2 keys (_id, name), got ${keys.length}: ${JSON.stringify(keys)}`)
      expect(keys).to.include('_id')
      expect(keys).to.include('name')
      cancelBody(res)
    })
  })

  describe('multiple subscribers same cohort', function () {
    it('should deliver events to two SSE subscribers on the same cohort', async function () {
      const res1 = await subscribeSSE('subscription { Product_added { _id name } }')
      const res2 = await subscribeSSE('subscription { Product_added { _id name } }')
      const event1Promise = waitForSSEEvent(res1.body,
        (e) => e.data?.Product_added, 5000)
      const event2Promise = waitForSSEEvent(res2.body,
        (e) => e.data?.Product_added, 5000)

      await document.insert(agent, {
        instance: [{
          '@type': 'Product',
          name: 'MultiSSE',
          description: 'For multiple subscribers',
        }],
      })

      const [event1, event2] = await Promise.all([event1Promise, event2Promise])
      expect(event1.data.Product_added._id).to.exist
      expect(event1.data.Product_added.name).to.equal('MultiSSE')
      expect(event2.data.Product_added._id).to.exist
      expect(event2.data.Product_added.name).to.equal('MultiSSE')
      cancelBody(res1)
      cancelBody(res2)
    })
  })

  describe('SSE event metadata', function () {
    it('should include _commit nested object with _id, _timestamp, _datetime, _change_type when explicitly requested', async function () {
      const res = await subscribeSSE('subscription { Product_added { _id name _commit { _id _timestamp _datetime _change_type } } }')
      const eventPromise = waitForSSEEvent(res.body,
        (e) => e.data?.Product_added, 5000)

      await document.insert(agent, {
        instance: [{
          '@type': 'Product',
          name: 'MetaEvent',
          description: 'For metadata check',
        }],
      })

      const event = await eventPromise
      // The _commit nested object should have all four fields.
      expect(event.data.Product_added._commit).to.exist
      expect(event.data.Product_added._commit._change_type).to.equal('added')
      expect(event.data.Product_added._commit._id).to.exist
      expect(event.data.Product_added._commit._timestamp).to.be.a('number')
      expect(event.data.Product_added._commit._datetime).to.be.a('string')
      // _datetime should be ISO8601 format (contains 'T' and 'Z').
      expect(event.data.Product_added._commit._datetime).to.match(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$/)
      // CRITICAL: exactly 3 top-level keys — selection set filtering must be exact.
      const keys = Object.keys(event.data.Product_added)
      expect(keys.length).to.equal(3, `expected exactly 3 keys (_id, name, _commit), got ${keys.length}: ${JSON.stringify(keys)}`)
      expect(keys).to.include('_id')
      expect(keys).to.include('name')
      expect(keys).to.include('_commit')
      // The _commit object should have exactly 4 keys.
      const commitKeys = Object.keys(event.data.Product_added._commit)
      expect(commitKeys.length).to.equal(4, `expected exactly 4 _commit keys, got ${commitKeys.length}: ${JSON.stringify(commitKeys)}`)
      // Old-style flat metadata names must NOT be present.
      expect(event.data.Product_added).to.not.have.property('_change_type')
      expect(event.data.Product_added).to.not.have.property('_commit_id')
      expect(event.data.Product_added).to.not.have.property('change_type')
      expect(event.data.Product_added).to.not.have.property('commit_id')
      cancelBody(res)
    })
  })

  describe('multiple documents in one commit', function () {
    it('should deliver separate events for two documents inserted in one commit', async function () {
      const res = await subscribeSSE('subscription { Product_added { _id name } }')
      // Use parseSSEStream which correctly handles the event: next format.
      const parser = parseSSEStream(res.body)

      // Give the subscription a moment to register, then insert two docs.
      await new Promise((resolve) => setTimeout(resolve, 200))
      await document.insert(agent, {
        instance: [
          { '@type': 'Product', name: 'FirstInCommit', description: 'first' },
          { '@type': 'Product', name: 'SecondInCommit', description: 'second' },
        ],
      })

      // Collect two events.
      const events = []
      const startTime = Date.now()
      while (events.length < 2 && Date.now() - startTime < 10000) {
        const event = await parser.next()
        if (event === null) break
        if (event?.data?.Product_added) {
          events.push(event)
        }
      }

      expect(events.length).to.be.at.least(2)
      const names = events.map((e) => e.data.Product_added.name).filter((n) => n)
      expect(names).to.include('FirstInCommit')
      expect(names).to.include('SecondInCommit')
      // Each event should have exactly 2 keys (_id, name).
      for (const event of events) {
        const keys = Object.keys(event.data.Product_added)
        expect(keys.length).to.equal(2, `expected exactly 2 keys, got ${keys.length}: ${JSON.stringify(keys)}`)
      }
      parser.cancel()
      cancelBody(res)
    })
  })

  describe('multiple commits on same connection', function () {
    it('should receive events from two separate commits on the same SSE connection', async function () {
      const res = await subscribeSSE('subscription { Product_added { _id name _commit { _id _change_type } } }')

      const parser = parseSSEStream(res.body)

      // Give the subscription a moment to register.
      await new Promise((resolve) => setTimeout(resolve, 200))

      // First commit: insert one document.
      await document.insert(agent, {
        instance: [{ '@type': 'Product', name: 'FirstCommit', description: 'first commit' }],
      })

      // Wait for the first event.
      const event1 = await waitForEventByName(parser, 'FirstCommit', 10000)
      expect(event1).to.exist
      expect(event1.data.Product_added.name).to.equal('FirstCommit')

      // Second commit: insert another document.
      await document.insert(agent, {
        instance: [{ '@type': 'Product', name: 'SecondCommit', description: 'second commit' }],
      })

      // Wait for the second event.
      const event2 = await waitForEventByName(parser, 'SecondCommit', 10000)
      expect(event2).to.exist
      expect(event2.data.Product_added.name).to.equal('SecondCommit')

      // The two events should have different _commit _id values (two separate commits).
      expect(event1.data.Product_added._commit._id).to.exist
      expect(event2.data.Product_added._commit._id).to.exist
      expect(event1.data.Product_added._commit._id).to.not.equal(event2.data.Product_added._commit._id)
      parser.cancel()
      cancelBody(res)
    })
  })

  describe('CORS headers', function () {
    it('should reflect Origin header in Access-Control-Allow-Origin', async function () {
      const controller = new AbortController()
      const auth = 'Basic ' + Buffer.from(agent.user + ':' + agent.password).toString('base64')
      const res = await fetch(sseUrl, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Accept: 'text/event-stream',
          Authorization: auth,
          Origin: 'https://example.com',
        },
        body: JSON.stringify({ query: 'subscription { Product_added { _id } }' }),
        signal: controller.signal,
      })
      expect(res.headers.get('access-control-allow-origin')).to.equal('https://example.com')
      expect(res.headers.get('access-control-allow-credentials')).to.equal('true')
      controller.abort()
    })

    it('should respond to OPTIONS preflight with 204 and CORS headers', async function () {
      const res = await fetch(sseUrl, {
        method: 'OPTIONS',
        headers: {
          Origin: 'https://example.com',
          'Access-Control-Request-Method': 'POST',
          'Access-Control-Request-Headers': 'authorization, content-type',
        },
      })
      expect(res.status).to.equal(204)
      expect(res.headers.get('access-control-allow-origin')).to.equal('https://example.com')
      expect(res.headers.get('access-control-allow-credentials')).to.equal('true')
    })
  })

  describe('delegation of non-streaming requests', function () {
    it('should delegate regular GraphQL query (Accept: application/json) to existing handler', async function () {
      const auth = 'Basic ' + Buffer.from(agent.user + ':' + agent.password).toString('base64')
      const res = await fetch(sseUrl, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Accept: 'application/json',
          Authorization: auth,
        },
        body: JSON.stringify({ query: '{ Product { _id name } }' }),
      })
      expect(res.status).to.equal(200)
      expect(res.headers.get('content-type')).to.include('application/json')
      const body = await res.json()
      // Regular GraphQL query response shape: { data: { Product: [...] } }
      expect(body.data).to.exist
      expect(body.data.Product).to.exist
    })

    it('should return _id in the same format as regular GraphQL query', async function () {
      // Insert a document to query.
      await document.insert(agent, {
        instance: [{ '@type': 'Product', name: 'IdConsistencyCheck', description: 'for id format' }],
      })

      // Query via regular GraphQL (delegation path).
      const auth = 'Basic ' + Buffer.from(agent.user + ':' + agent.password).toString('base64')
      const queryRes = await fetch(sseUrl, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Accept: 'application/json',
          Authorization: auth,
        },
        body: JSON.stringify({ query: '{ Product { _id name } }' }),
      })
      const queryBody = await queryRes.json()
      const queryDoc = queryBody.data.Product.find((d) => d.name === 'IdConsistencyCheck')
      expect(queryDoc).to.exist
      const queryId = queryDoc._id

      // Subscribe via SSE and get the event for the same document type.
      const res = await subscribeSSE('subscription { Product_added { _id name } }')
      const eventPromise = waitForSSEEvent(res.body,
        (e) => e.data?.Product_added && e.data?.Product_added.name === 'IdConsistencyCheck', 5000)

      // Insert another doc with the same name (triggers _added event).
      // First delete the old one, then re-insert to get a fresh _added event.
      await document.delete(agent, { query: { id: queryId } })
      await document.insert(agent, {
        instance: [{ '@type': 'Product', name: 'IdConsistencyCheck', description: 'for id format 2' }],
      })

      const event = await eventPromise
      const sseId = event.data.Product_added._id

      // Both should use the same _id format (expanded IRI or compressed).
      // The GraphQL resolver returns expanded IRIs (terminusdb:///data/Product/...).
      // The SSE event _id comes from the same resolver, so they should match in format.
      expect(sseId).to.include('Product/')
      // Check that the format matches: both are expanded IRIs.
      const queryIdHasPrefix = queryId.startsWith('terminusdb:///data/')
      const sseIdHasPrefix = sseId.startsWith('terminusdb:///data/')
      expect(queryIdHasPrefix).to.equal(sseIdHasPrefix)
      cancelBody(res)
    })
  })

  describe('system database subscriptions', function () {
    let systemAgent
    let systemSseUrl

    before(async function () {
      systemAgent = new Agent()
      systemAgent.auth()
      systemSseUrl = systemAgent.baseUrl + '/api/graphql/_system'
    })

    async function subscribeSystemSSE (query) {
      const controller = new AbortController()
      const auth = 'Basic ' + Buffer.from(systemAgent.user + ':' + systemAgent.password).toString('base64')
      const res = await fetch(systemSseUrl, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Accept: 'text/event-stream',
          Authorization: auth,
        },
        body: JSON.stringify({ query }),
        signal: controller.signal,
      })
      res._abortController = controller
      return res
    }

    it('should receive UserDatabase_added event when a database is created', async function () {
      const res = await subscribeSystemSSE('subscription { UserDatabase_added { _id name } }')
      expect(res.status).to.equal(200)
      expect(res.headers.get('content-type')).to.include('text/event-stream')

      const eventPromise = waitForSSEEvent(res.body,
        (e) => e.data?.UserDatabase_added, 10000)

      const sysDbAgent = new Agent()
      sysDbAgent.auth()
      await db.create(sysDbAgent, { label: 'System SSE Subscription Test DB', schema: true })

      const event = await eventPromise
      expect(event.data.UserDatabase_added).to.exist
      expect(event.data.UserDatabase_added._id).to.exist
      expect(event.data.UserDatabase_added.name).to.be.a('string')

      cancelBody(res)
      await db.delete(sysDbAgent)
    })
  })
})
