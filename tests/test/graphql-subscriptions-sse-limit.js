const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

function cancelBody (res) {
  try { res?.body?.cancel?.().catch?.(() => {}) } catch { /* already closed */ }
  try { res?._abortController?.abort?.() } catch { /* already closed */ }
}

// The per-user cap, as `SubscriptionLimiter::default` reads it.
const LIMIT = Number(process.env.TERMINUSDB_SSE_MAX_SUBSCRIPTIONS_PER_USER) || 20

describe('GraphQL SSE subscription limit', function () {
  this.timeout(60000)
  let agent
  let sseUrl

  const schema = [{
    '@type': '@context',
    '@base': 'terminusdb:///data/',
    '@schema': 'terminusdb:///schema#',
  }, {
    '@id': 'Product',
    '@type': 'Class',
    '@key': { '@type': 'Lexical', '@fields': ['name'] },
    name: 'xsd:string',
  }]

  before(async function () {
    this.timeout(30000)
    agent = new Agent()
    agent.auth()
    await db.create(agent, { label: 'GraphQL SSE Subscription Limit Test', schema: true })
    await document.insert(agent, { schema, fullReplace: true })
    sseUrl = agent.baseUrl + '/api/graphql/' +
      agent.orgName + '/' + agent.dbName + '/local/branch/main'
  })

  after(async function () {
    this.timeout(30000)
    await db.delete(agent)
  })

  async function subscribeSSE () {
    const controller = new AbortController()
    const auth = 'Basic ' + Buffer.from(agent.user + ':' + agent.password).toString('base64')
    const res = await fetch(sseUrl, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        Accept: 'text/event-stream',
        Authorization: auth,
      },
      body: JSON.stringify({ query: 'subscription { Product_added { _id } }' }),
      signal: controller.signal,
    })
    res._abortController = controller
    return res
  }

  // One test, in two phases, because the budget is per *user* and both phases
  // share it: splitting them would make the second fail for a reason that has
  // nothing to do with what it asserts.
  //
  // Phase one asserts the cap works and passes today. Phase two asserts that
  // dropping those streams gives the budget back, and does not: a slot acquired
  // by `appserver_check_subscription_limit/1` is released only by
  // `cleanup_sse_stream/1`, reached only from `sweep_stale_sse_streams/0`, which
  // treats a stream as stale only when `appserver_stream_exists/1` fails — and
  // that consults `stream_registry`, pruned in exactly one place:
  // `StreamRegistry::send` dropping a sender whose send failed. SSE broadcasts
  // never go through it (`appserver_broadcast_send_raw` publishes to a tokio
  // broadcast channel, and per-stream forwarder tasks write to a cloned
  // `mpsc::Sender`), so a vanished client's id stays in the registry and its
  // slot is never returned. Commits do not recover it, and neither does the 90s
  // idle timeout: that aborts the forwarder task only.
  it('releases a subscription slot when the subscriber goes away', async function () {
    // Phase one: the cap is a cap.
    const held = []
    try {
      for (let n = 1; n <= LIMIT; n += 1) {
        const res = await subscribeSSE()
        expect(res.status, `holding ${n} of ${LIMIT} open`).to.equal(200)
        expect(res.headers.get('content-type')).to.include('text/event-stream')
        held.push(res)
      }
      const refused = await subscribeSSE()
      expect(
        refused.status,
        'a client holding its whole budget open must be refused',
      ).to.equal(429)
      const body = await refused.json()
      expect(JSON.stringify(body)).to.include('TOO_MANY_REQUESTS')
    } finally {
      for (const res of held) cancelBody(res)
    }

    // Phase two: every one of those subscribers is now gone, so the budget
    // should be spendable again. Retried, to allow for reclamation that is
    // asynchronous rather than absent.
    let status = null
    for (let attempt = 1; attempt <= 10; attempt += 1) {
      const res = await subscribeSSE()
      status = res.status
      if (status === 200) { cancelBody(res); break }
      await new Promise((resolve) => setTimeout(resolve, 500))
    }
    expect(
      status,
      `all ${LIMIT} subscribers were dropped, but the budget is still spent: their slots ` +
      'are held by clients that are gone, and nothing reclaims them — not a commit, not the ' +
      'idle timeout. See the comment above this test for where the chain breaks.',
    ).to.equal(200)
  })
})
