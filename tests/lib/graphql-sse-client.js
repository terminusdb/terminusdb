#!/usr/bin/env node

/**
 * Standalone GraphQL Subscription SSE Client for TerminusDB.
 *
 * Usage:
 *   node tests/lib/graphql-sse-client.js
 *
 * Tweak the CONFIG section below to change the server URL, credentials,
 * and subscription query.
 *
 * The SSE endpoint is the regular GraphQL endpoint with an
 * Accept: text/event-stream header:
 *   POST /api/graphql/<org>/<db>/local/branch/<branch>
 *
 * Examples:
 *   http://localhost:7373/api/graphql/admin/product_assortment
 *   http://localhost:6363/api/graphql/admin/system
 */

// ─── CONFIG ─────────────────────────────────────────────────────────────

const CONFIG = {
  // Server URL — short paths like "admin/db" work (defaults to local/branch/main)
  // Or use full path: "admin/product_assortment/local/branch/main"
  url: 'http://localhost:7373/api/graphql/admin/product_assortment',

  // Credentials (Basic auth)
  user: 'admin',
  password: 'root',

  // Subscription query — change this to test different operations
  query: 'subscription { MyClass_changed { _id name } }',
}

// ─── CLIENT ─────────────────────────────────────────────────────────────

const authHeader = 'Basic ' + Buffer.from(`${CONFIG.user}:${CONFIG.password}`).toString('base64')

console.log(`Connecting to ${CONFIG.url}`)

const controller = new AbortController()

function handleSSEEvent (event) {
  if (event.startsWith(':')) {
    console.log(`[comment] ${event.trim()}`)
    return
  }
  if (event.startsWith('data: ')) {
    const json = event.slice(6)
    try {
      const parsed = JSON.parse(json)
      console.log('[event]', JSON.stringify(parsed, null, 2))
    } catch {
      console.log('[event:raw]', json)
    }
  }
}

async function run () {
  const res = await fetch(CONFIG.url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Accept: 'text/event-stream',
      Authorization: authHeader,
    },
    body: JSON.stringify({ query: CONFIG.query }),
    signal: controller.signal,
  })

  if (!res.ok) {
    console.error(`[error] HTTP ${res.status} ${res.statusText}`)
    const text = await res.text()
    console.error(text)
    process.exit(1)
  }

  console.log(`[connected] ${res.headers.get('content-type')}`)

  const reader = res.body.getReader()
  const decoder = new TextDecoder()
  let buffer = ''

  try {
    while (true) {
      const { done, value } = await reader.read()
      if (done) break
      buffer += decoder.decode(value, { stream: true })
      const events = buffer.split('\n\n')
      buffer = events.pop()
      for (const event of events) {
        handleSSEEvent(event)
      }
    }
  } finally {
    console.log('[stream ended]')
  }
}

run().catch((err) => {
  if (err.name === 'AbortError') {
    console.log('\n[shutdown] Aborted')
  } else {
    console.error('[error]', err.message)
    process.exit(1)
  }
})

// Graceful shutdown on Ctrl-C
process.on('SIGINT', () => {
  console.log('\n[shutdown] Aborting SSE stream')
  controller.abort()
})
