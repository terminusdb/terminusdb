const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')

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
    age: 'xsd:decimal',
  }]

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
    await document.insert(agent, { schema, fullReplace: true })
  })

  after(async function () {
    // await db.delete(agent)
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
    // Start SSE subscription
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

    // Read the stream until we get an event
    const reader = response.body.getReader()
    const decoder = new TextDecoder()
    let receivedData = null
    let buffer = ''
    let connected = false

    // Read chunks until we find the "connected" event, then insert
    const readTimeout = new Promise((_resolve, reject) =>
      setTimeout(() => reject(new Error('SSE read timeout')), 15000),
    )

    try {
      // First, wait for the "connected" event from the server
      while (!connected) {
        const { done, value } = await Promise.race([
          reader.read(),
          readTimeout,
        ])
        if (done) break
        buffer += decoder.decode(value, { stream: true })
        const events = buffer.split('\n\n')
        buffer = events.pop()
        for (const block of events) {
          if (block.startsWith('event: connected')) {
            connected = true
            break
          }
        }
      }
    } finally {
      // not here — we continue reading after insert
    }

    // Now insert a document to trigger the subscription event
    const insertPromise = document.insert(agent, {
      instance: [{ '@type': 'Person', name: 'SSETestPerson' }],
    })

    try {
      while (receivedData === null) {
        const { done, value } = await Promise.race([
          reader.read(),
          readTimeout,
        ])
        if (done) break
        buffer += decoder.decode(value, { stream: true })
        // Look for SSE event format: "event: next\ndata: {...}\n\n"
        const dataMatch = /data: (.+)/.exec(buffer)
        if (dataMatch) {
          receivedData = dataMatch[1]
          break
        }
      }
    } finally {
      controller.abort()
    }

    // Wait for insert to complete
    await insertPromise

    expect(receivedData).to.not.be.null
    const parsed = JSON.parse(receivedData)
    expect(parsed).to.have.property('data')
    expect(parsed.data).to.have.property('Person_added')
    expect(parsed.data.Person_added).to.have.property('name')
    expect(parsed.data.Person_added.name).to.equal('SSETestPerson')
  })

  it('SSE subscription includes _commit metadata when requested', async function () {
    const controller = new AbortController()
    const response = await fetch(graphqlUrl(), {
      method: 'POST',
      headers: {
        ...authHeaders(),
        Accept: 'text/event-stream',
      },
      body: JSON.stringify({
        query: 'subscription { Person_added { _id name _commit { _id _change_type } } }',
      }),
      signal: controller.signal,
    })
    expect(response.status).to.equal(200)

    const reader = response.body.getReader()
    const decoder = new TextDecoder()
    let receivedData = null
    let buffer = ''
    let connected = false

    const readTimeout = new Promise((_resolve, reject) =>
      setTimeout(() => reject(new Error('SSE read timeout')), 15000),
    )

    try {
      // First, wait for the "connected" event from the server
      while (!connected) {
        const { done, value } = await Promise.race([
          reader.read(),
          readTimeout,
        ])
        if (done) break
        buffer += decoder.decode(value, { stream: true })
        const events = buffer.split('\n\n')
        buffer = events.pop()
        for (const block of events) {
          if (block.startsWith('event: connected')) {
            connected = true
            break
          }
        }
      }
    } finally {
      // continue reading after insert
    }

    const insertPromise = document.insert(agent, {
      instance: [{ '@type': 'Person', name: 'SSECommitTest' }],
    })

    try {
      while (receivedData === null) {
        const { done, value } = await Promise.race([
          reader.read(),
          readTimeout,
        ])
        if (done) break
        buffer += decoder.decode(value, { stream: true })
        const dataMatch = /data: (.+)/.exec(buffer)
        if (dataMatch) {
          receivedData = dataMatch[1]
          break
        }
      }
    } finally {
      controller.abort()
    }

    await insertPromise

    expect(receivedData).to.not.be.null
    const parsed = JSON.parse(receivedData)
    expect(parsed.data.Person_added).to.have.property('_commit')
    expect(parsed.data.Person_added._commit).to.have.property('_id')
    expect(parsed.data.Person_added._commit).to.have.property('_change_type', 'added')
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
