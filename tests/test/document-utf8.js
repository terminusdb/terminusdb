const { expect } = require('chai')
const { Agent, api, db, document } = require('../lib')

// Regression tests for: non-ASCII characters silently rejected by document API.
//
// Root cause: http_read_json_stream_for_documents_body in routes.pl calls
// open_string/2, which creates an iso_latin_1 stream by default. When the
// HTTP client declares "Content-Type: application/json; charset=utf-8",
// SWI-Prolog's http_read_data decodes the payload as UTF-8 into a Prolog
// string, then open_string re-encodes it as iso_latin_1. Multi-byte UTF-8
// sequences (e.g. ü = 0xC3 0xBC) become the single Latin-1 byte 0xFC, which
// is invalid UTF-8. The Rust serde_json parser throws, the blanket
// catch(_Error, fail) in json_read_dict_stream silently converts the error to
// EOF, and the document is dropped — returning [] with no error.
//
// Trigger condition: the bug fires when Content-Type includes "; charset=utf-8".
// Without it, http_read_data takes a different internal path and the bytes
// arrive correctly. Conforming HTTP clients (including curl and many libraries)
// correctly declare charset=utf-8 when sending UTF-8 JSON.
//
// Note: mitmproxy (used in the dev stack on port 6363) masks this bug because
// Python's json.dumps re-encodes non-ASCII as \uXXXX escape sequences before
// forwarding. These tests must run against a direct TDB server, not the proxy.

describe('document-utf8', function () {
  let agent

  const schema = {
    '@id': 'Utf8Item',
    '@type': 'Class',
    '@key': { '@type': 'Random' },
    note: 'xsd:string',
  }

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
    await document.insert(agent, { schema })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('inserts a document with raw UTF-8 bytes and Content-Type charset=utf-8', async function () {
    // This is the exact trigger: charset=utf-8 in Content-Type, raw UTF-8 bytes in body.
    // When the bug is present, TDB returns [] and the length assertion fails.
    const noteValue = 'Düsseldorf: ü ö ä'
    const rawBody = Buffer.from(
      JSON.stringify([{ '@type': 'Utf8Item', note: noteValue }]),
      'utf8',
    )

    const insertResponse = await agent
      .post(api.path.document(agent))
      .query({ graph_type: 'instance', author: 'test', message: 'utf8-insert' })
      .set('Content-Type', 'application/json; charset=utf-8')
      .serialize((data) => data)
      .send(rawBody)

    expect(insertResponse.status, 'insert should succeed').to.equal(200)
    expect(
      insertResponse.body,
      'expected one ID returned — [] means the document was silently dropped (UTF-8 bug)',
    ).to.be.an('array').with.lengthOf(1)

    const insertedId = insertResponse.body[0]
    expect(insertedId).to.be.a('string').and.not.empty

    // Read back and verify the value survived the round-trip unmangled.
    const getResponse = await agent
      .get(api.path.document(agent))
      .query({ id: insertedId })

    expect(getResponse.status).to.equal(200)
    const doc = Array.isArray(getResponse.body) ? getResponse.body[0] : getResponse.body
    expect(doc).to.have.property('note', noteValue)
  })

  it('inserts a document with raw UTF-8 bytes and no charset declaration (control — must also pass)', async function () {
    // Without charset=utf-8, http_read_data takes a different path and succeeds.
    // This test confirms the control case and will catch any regression that
    // breaks the no-charset path.
    const noteValue = 'München: Straße, weiß'
    const rawBody = Buffer.from(
      JSON.stringify([{ '@type': 'Utf8Item', note: noteValue }]),
      'utf8',
    )

    const insertResponse = await agent
      .post(api.path.document(agent))
      .query({ graph_type: 'instance', author: 'test', message: 'utf8-nochars' })
      .set('Content-Type', 'application/json')
      .serialize((data) => data)
      .send(rawBody)

    expect(insertResponse.status).to.equal(200)
    expect(insertResponse.body).to.be.an('array').with.lengthOf(1)

    const insertedId = insertResponse.body[0]

    const getResponse = await agent
      .get(api.path.document(agent))
      .query({ id: insertedId })

    expect(getResponse.status).to.equal(200)
    const doc = Array.isArray(getResponse.body) ? getResponse.body[0] : getResponse.body
    expect(doc).to.have.property('note', noteValue)
  })
})
