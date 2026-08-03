const { expect } = require('chai')
const { Agent, api, db, document, optimize, util } = require('../lib')

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
  if (dataLine.trim() === '') return { _eventType: eventType, data: null }
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
    setTimeout(() => { parser.cancel(); reject(new Error('SSE timeout waiting for matching event')) }, timeoutMs || 15000))
  const search = (async () => {
    while (true) {
      const event = await parser.next()
      if (event === null) throw new Error('SSE stream ended without a matching event')
      if (predicate(event)) { parser.cancel(); return event }
    }
  })()
  return Promise.race([search, timeout])
}

describe('GraphQL Subscriptions SSE', function () {
  let agent

  const schema = [{
    '@type': '@context',
    '@base': 'terminusdb:///data/',
    '@schema': 'terminusdb:///schema#',
  }, {
    '@id': 'Person',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    age: { '@type': 'Optional', '@class': 'xsd:decimal' },
  }]

  before(async function () {
    this.timeout(180000)
    agent = new Agent().auth()
    await db.create(agent)
    await document.insert(agent, { schema, fullReplace: true })
    // Squash the commit graph after schema insert for deterministic performance.
    const dbPath = `${agent.orgName}/${agent.dbName}`
    await optimize.optimizeDatabase(agent, dbPath, 'main')
  })

  after(async function () {
    this.timeout(30000)
    await db.delete(agent)
  })

  function graphqlEndpoint () {
    return api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName })
  }

  function graphqlUrl () {
    return `${agent.baseUrl}${graphqlEndpoint()}`
  }

  function authHeaders () {
    return {
      Authorization: util.authorizationHeader(agent),
      'Content-Type': 'application/json',
    }
  }

  it('OPTIONS preflight returns 204 with CORS headers', async function () {
    const response = await fetch(graphqlUrl(), {
      method: 'OPTIONS',
      headers: {
        Origin: 'https://example.com',
        'Access-Control-Request-Method': 'POST',
        'Access-Control-Request-Headers': 'content-type, authorization',
      },
    })
    expect(response.status).to.equal(204)
    expect(response.headers.get('Access-Control-Allow-Origin')).to.equal('https://example.com')
    expect(response.headers.get('Access-Control-Allow-Credentials')).to.equal('true')
    expect(response.headers.get('Access-Control-Allow-Methods')).to.include('POST')
  })

  it('non-streaming request delegates to regular GraphQL handler', async function () {
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: authHeaders(),
      body: JSON.stringify({
        query: '{ Person { name } }',
      }),
    })
    expect(response.status).to.equal(200)
    expect(response.headers.get('Content-Type')).to.include('application/json')
    const body = await response.json()
    expect(body).to.have.property('data')
    expect(body.data).to.have.property('Person')
    expect(body.data.Person).to.be.an('array')
  })

  it('SSE subscription returns 200 with text/event-stream content type', async function () {
    const controller = new AbortController()
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: {
        ...authHeaders(),
        Accept: 'text/event-stream',
      },
      body: JSON.stringify({
        query: 'subscription { Person_added { _id name } }',
      }),
      signal: controller.signal,
    })
    expect(response.status).to.equal(200)
    expect(response.headers.get('Content-Type')).to.equal('text/event-stream')
    expect(response.headers.get('Cache-Control')).to.equal('no-cache')
    expect(response.headers.get('X-Accel-Buffering')).to.equal('no')

    // Abort the fetch to close the underlying TCP connection
    controller.abort()
  })

  it('NDJSON subscription returns 200 with application/x-ndjson content type', async function () {
    const controller = new AbortController()
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: {
        ...authHeaders(),
        Accept: 'application/x-ndjson',
      },
      body: JSON.stringify({
        query: 'subscription { Person_added { _id name } }',
      }),
      signal: controller.signal,
    })
    expect(response.status).to.equal(200)
    expect(response.headers.get('Content-Type')).to.equal('application/x-ndjson')
    expect(response.headers.get('Cache-Control')).to.equal('no-cache')

    controller.abort()
  })

  it('SSE subscription delivers event after document insert', async function () {
    const controller = new AbortController()
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: {
        ...authHeaders(),
        Accept: 'text/event-stream',
      },
      body: JSON.stringify({
        query: 'subscription { Person_added { _id name } }',
      }),
      signal: controller.signal,
    })
    expect(response.status).to.equal(200)

    const eventPromise = waitForSSEEvent(response.body,
      (e) => e._eventType === 'next' && e.data?.Person_added, 15000)

    await document.insert(agent, {
      instance: [{ '@type': 'Person', name: 'SSETestPerson' }],
    })

    const event = await eventPromise
    controller.abort()

    expect(event.data).to.have.property('Person_added')
    expect(event.data.Person_added).to.have.property('name')
    expect(event.data.Person_added.name).to.equal('SSETestPerson')
  })

  it('SSE subscription includes _CommitMetadata metadata when requested', async function () {
    const controller = new AbortController()
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: {
        ...authHeaders(),
        Accept: 'text/event-stream',
      },
      body: JSON.stringify({
        query: 'subscription { Person_added { _id name _CommitMetadata { _id _change_type } } }',
      }),
      signal: controller.signal,
    })
    expect(response.status).to.equal(200)

    const eventPromise = waitForSSEEvent(response.body,
      (e) => e._eventType === 'next' && e.data?.Person_added?._CommitMetadata, 15000)

    await document.insert(agent, {
      instance: [{ '@type': 'Person', name: 'SSECommitTest' }],
    })

    const event = await eventPromise
    controller.abort()

    expect(event.data.Person_added).to.have.property('_CommitMetadata')
    expect(event.data.Person_added._CommitMetadata).to.have.property('_id')
    expect(event.data.Person_added._CommitMetadata).to.have.property('_change_type', 'added')
  })

  it('returns 400 for invalid subscription query body', async function () {
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: {
        ...authHeaders(),
        Accept: 'text/event-stream',
      },
      body: 'not json',
    })
    expect(response.status).to.equal(400)
    const body = await response.text()
    expect(body).to.include('invalid_query_body')
  })

  it('emits validation error as next event for invalid GraphQL subscription query', async function () {
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        ...authHeaders(),
        Accept: 'text/event-stream',
      },
      body: JSON.stringify({
        query: 'subscription { InvalidSyntax {{{ } }',
      }),
    })
    expect(response.status).to.equal(200)
    expect(response.headers.get('content-type')).to.include('text/event-stream')
    const parser = parseSSEStream(response.body)
    const events = []
    const collectTimeout = new Promise((_resolve, reject) =>
      setTimeout(() => { parser.cancel(); reject(new Error('SSE timeout')) }, 10000))
    await Promise.race([
      (async () => {
        while (true) {
          const event = await parser.next()
          if (event === null) break
          events.push(event)
        }
      })(),
      collectTimeout,
    ])
    const nextEvent = events.find(e => e._eventType === 'next')
    expect(nextEvent).to.exist
    expect(JSON.stringify(nextEvent)).to.include('errors')
    const completeEvent = events.find(e => e._eventType === 'complete')
    expect(completeEvent).to.exist
  })

  it('executes finite GraphQL query over SSE with next and complete events', async function () {
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        ...authHeaders(),
        Accept: 'text/event-stream',
      },
      body: JSON.stringify({
        query: '{ Person { _id name } }',
      }),
    })
    expect(response.status).to.equal(200)
    expect(response.headers.get('content-type')).to.include('text/event-stream')
    const parser = parseSSEStream(response.body)
    const events = []
    const collectTimeout = new Promise((_resolve, reject) =>
      setTimeout(() => { parser.cancel(); reject(new Error('SSE timeout')) }, 10000))
    await Promise.race([
      (async () => {
        while (true) {
          const event = await parser.next()
          if (event === null) break
          events.push(event)
        }
      })(),
      collectTimeout,
    ])
    const nextEvent = events.find(e => e._eventType === 'next')
    expect(nextEvent).to.exist
    expect(JSON.stringify(nextEvent)).to.include('Person')
    const completeEvent = events.find(e => e._eventType === 'complete')
    expect(completeEvent).to.exist
  })

  it('returns 401 without authentication', async function () {
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        Accept: 'text/event-stream',
      },
      body: JSON.stringify({
        query: 'subscription { Person_added { _id } }',
      }),
    })
    expect(response.status).to.equal(401)
  })
})
