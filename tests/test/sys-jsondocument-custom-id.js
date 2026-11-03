const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('node-fetch')

describe('sys:JSONDocument Custom IRI @id Support', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent, { label: 'JSONDocument Custom ID Test', schema: false })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('should accept external IRI as @id (https://example.com/myid)', async function () {
    const originalData = {
      '@id': 'https://example.com/myid',
      name: 'example_document',
      data: {
        message: 'Hello World',
        count: 42,
        nested: {
          value: 123.45,
        },
      },
    }

    const result = await document.insert(agent, {
      instance: originalData,
      author: 'test_author',
      raw_json: true,
    })

    expect(result.body).to.be.an('array').with.lengthOf(1)
    const returnedId = result.body[0]

    // External IRI should be preserved as-is
    expect(returnedId).to.equal('https://example.com/myid')

    // Retrieve and verify
    const retrieved = await document.get(agent, { query: { id: returnedId } })
    expect(retrieved.body['@id']).to.equal('https://example.com/myid')
    expect(retrieved.body.name).to.equal('example_document')
    expect(retrieved.body.data.message).to.equal('Hello World')
    expect(retrieved.body.data.count).to.equal(42)
    expect(retrieved.body.data.nested.value).to.equal(123.45)
  })

  it('should accept terminusdb:// prefixed IRI', async function () {
    const originalData = {
      '@id': 'terminusdb:///data/docs/doc123',
      name: 'terminusdb_prefixed',
      value: 'test',
    }

    const result = await document.insert(agent, {
      instance: originalData,
      author: 'test_author',
      raw_json: true,
    })

    expect(result.body).to.be.an('array').with.lengthOf(1)
    const returnedId = result.body[0]

    expect(returnedId).to.equal('terminusdb:///data/docs/doc123')

    const retrieved = await document.get(agent, { query: { id: returnedId } })
    expect(retrieved.body.name).to.equal('terminusdb_prefixed')
    expect(retrieved.body.value).to.equal('test')
  })

  it('should accept plain string @id and expand with @base directly', async function () {
    const originalData = {
      '@id': 'my-simple-doc',
      name: 'Simple Document',
      content: 'Plain string ID',
    }

    const result = await document.insert(agent, {
      instance: originalData,
      author: 'test_author',
      raw_json: true,
    })

    expect(result.body).to.be.an('array').with.lengthOf(1)
    const returnedId = result.body[0]

    // Plain string should get @base prepended directly (no JSONDocument/ prefix)
    expect(returnedId).to.include('terminusdb:///data/')
    expect(returnedId).to.include('my-simple-doc')
    expect(returnedId).to.not.include('JSONDocument')

    // Retrieve and verify
    const retrieved = await document.get(agent, { query: { id: returnedId } })
    expect(retrieved.body.name).to.equal('Simple Document')
    expect(retrieved.body.content).to.equal('Plain string ID')
  })

  it('should handle @@id in nested sys:JSON as xsd:anyURI', async function () {
    const originalData = {
      '@id': 'https://example.com/parent',
      name: 'parent_document',
      nested: {
        '@@id': 'https://example.com/child',
        '@@type': 'https://schema.org/Thing',
        value: 'nested data',
      },
    }

    const result = await document.insert(agent, {
      instance: originalData,
      author: 'test_author',
      raw_json: true,
    })

    expect(result.body).to.be.an('array').with.lengthOf(1)
    const returnedId = result.body[0]

    // Retrieve and verify @@id/@@type are preserved as IRIs
    const retrieved = await document.get(agent, { query: { id: returnedId } })
    expect(retrieved.body.nested['@id']).to.equal('https://example.com/child')
    expect(retrieved.body.nested['@type']).to.equal('https://schema.org/Thing')
    expect(retrieved.body.nested.value).to.equal('nested data')
  })

  it('should handle known prefixed IRI as @id', async function () {
    // First add a custom prefix to the context (this would normally be done via schema)
    // For now, just test with terminusdb:/// which is always available
    const originalData = {
      '@id': 'terminusdb:///data/custom-doc-123',
      name: 'prefixed_document',
      value: 'test',
    }

    const result = await document.insert(agent, {
      instance: originalData,
      author: 'test_author',
      raw_json: true,
    })

    expect(result.body).to.be.an('array').with.lengthOf(1)
    const returnedId = result.body[0]

    // Verify the ID is preserved
    expect(returnedId).to.equal('terminusdb:///data/custom-doc-123')

    // Retrieve and verify
    const retrieved = await document.get(agent, { query: { id: returnedId } })
    expect(retrieved.body.name).to.equal('prefixed_document')
    expect(retrieved.body.value).to.equal('test')
  })

  it('should handle user-chosen path components like JSONDocument/', async function () {
    const originalData = {
      '@id': 'JSONDocument/my-prefixed-doc',
      name: 'User Controlled Path',
      content: 'JSONDocument is just a regular path component',
    }

    const result = await document.insert(agent, {
      instance: originalData,
      author: 'test_author',
      raw_json: true,
    })

    expect(result.body).to.be.an('array').with.lengthOf(1)
    const returnedId = result.body[0]

    // JSONDocument/ is treated as a regular path component, prepended with @base
    expect(returnedId).to.include('terminusdb:///data/JSONDocument/my-prefixed-doc')

    // Retrieve with the correct ID
    const retrieved = await document.get(agent, { query: { id: returnedId } })
    expect(retrieved.body.name).to.equal('User Controlled Path')
    expect(retrieved.body.content).to.equal('JSONDocument is just a regular path component')
  })

  it('should handle plain string ID consistently on insert and retrieve', async function () {
    const originalData = {
      '@id': 'consistent-doc',
      name: 'Consistency Test',
    }

    const insertResult = await document.insert(agent, {
      instance: originalData,
      author: 'test_author',
      raw_json: true,
    })

    const insertedId = insertResult.body[0]

    // Should be able to retrieve with the returned ID
    const retrieved = await document.get(agent, { query: { id: insertedId } })
    // The retrieved @id uses @base directly (no automatic JSONDocument/ prefix)
    expect(retrieved.body['@id']).to.include('consistent-doc')
    expect(retrieved.body.name).to.equal('Consistency Test')
  })

  it('should omit @id and @type on read when raw_id=false', async function () {
    const insertResult = await document.insert(agent, {
      instance: { name: 'no_internal_mechanics', n: 7 },
      author: 'test_author',
      raw_json: true,
    })
    const id = insertResult.body[0]

    const retrieved = await document.get(agent, {
      query: { id, raw_id: false },
    })
    expect(retrieved.body).to.not.have.property('@id')
    expect(retrieved.body).to.not.have.property('@type')
    expect(retrieved.body.name).to.equal('no_internal_mechanics')
    expect(retrieved.body.n).to.equal(7)
  })

  it('should include @id on read when raw_id=true (default)', async function () {
    const insertResult = await document.insert(agent, {
      instance: { name: 'with_id' },
      author: 'test_author',
      raw_json: true,
    })
    const id = insertResult.body[0]

    const explicit = await document.get(agent, {
      query: { id, raw_id: true, compress_ids: false },
    })
    expect(explicit.body['@id']).to.equal(id)

    const implicit = await document.get(agent, { query: { id } })
    expect(implicit.body['@id']).to.include('JSONDocument/')
  })

  it('should omit @id in as_list reads when raw_id=false', async function () {
    await document.insert(agent, {
      instance: { name: 'list_member' },
      author: 'test_author',
      raw_json: true,
    })

    const retrieved = await document.get(agent, {
      query: { as_list: true, raw_id: false },
    })
    expect(retrieved.body).to.be.an('array')
    const doc = retrieved.body.find((d) => d.name === 'list_member')
    expect(doc).to.exist
    expect(doc).to.not.have.property('@id')
    expect(doc).to.not.have.property('@type')
  })

  it('should use the id parameter to name a raw JSON document on insert', async function () {
    const result = await document.insert(agent, {
      instance: { name: 'url_named' },
      author: 'test_author',
      queryString: 'raw_json=true&id=url-named-doc&author=test_author&message=test&compress_ids=false',
    })
    expect(result.body[0]).to.equal('terminusdb:///data/url-named-doc')

    const retrieved = await document.get(agent, {
      query: { id: 'terminusdb:///data/url-named-doc' },
    })
    expect(retrieved.body.name).to.equal('url_named')
  })

  it('should fail when the id parameter conflicts with the body @id', async function () {
    await document.insert(agent, {
      instance: { '@id': 'body-doc', name: 'conflict' },
      author: 'test_author',
      queryString: 'raw_json=true&id=other-doc&author=test_author&message=test',
    }).fails(api.error.conflictingDocumentId('other-doc', 'body-doc'))
  })

  it('should fail when the id parameter is combined with multiple documents', async function () {
    await document.insert(agent, {
      instance: [{ name: 'one' }, { name: 'two' }],
      author: 'test_author',
      queryString: 'raw_json=true&id=multi-doc&author=test_author&message=test',
    }).fails()
  })

  it('should replace a raw JSON document by id parameter without body @id', async function () {
    const insertResult = await document.insert(agent, {
      instance: { name: 'replace_me', v: 1 },
      author: 'test_author',
      raw_json: true,
    })
    const id = insertResult.body[0]

    const replaced = await document.replace(agent, {
      instance: { name: 'replace_me', v: 2 },
      author: 'test_author',
      queryString: `raw_json=true&id=${encodeURIComponent(id)}&author=test_author&message=test&compress_ids=false`,
    })
    expect(replaced.body[0]).to.equal(id)

    const retrieved = await document.get(agent, {
      query: { id, raw_id: false },
    })
    expect(retrieved.body).to.not.have.property('@id')
    expect(retrieved.body.v).to.equal(2)
  })

  it('should preserve high-precision numbers byte-identically with and without raw_id', async function () {
    // bodyString keeps the literal request bytes so ingest sees the full
    // precision, unaffected by JSON.stringify. dec20 has 20 significant
    // decimal places (leading zero after the point avoids trailing-zero
    // trimming); int29 exceeds u64/f64 range entirely.
    const insertResult = await document.insert(agent, {
      bodyString: '{"dec20":0.01234567890123456789,"int29":12345678901234567890123456789,"neg":-0.000001,"small":1.5e-10}',
      author: 'test_author',
      queryString: 'raw_json=true&id=precision-doc&author=test_author&message=test',
    })
    const id = insertResult.body[0]

    // Fetch raw response text for both paths; JSON.parse would truncate
    // the precision we are asserting. Mirrors the technique in
    // sys-json-decimal-type.js.
    const fetchRaw = async (rawId) => {
      const response = await fetch(
        `${agent.baseUrl}${api.path.document(agent)}?id=${encodeURIComponent(id)}&raw_id=${rawId}`,
        { headers: { Authorization: util.authorizationHeader(agent) } },
      )
      expect(response.ok).to.be.true
      return response.text()
    }
    const withIdText = await fetchRaw(true)
    const withoutIdText = await fetchRaw(false)

    // Why byte parity is asserted, not just numeric equality: an earlier
    // implementation of raw_id=false re-serialized the document through
    // Prolog's json_write_dict instead of the Rust FFI, which rendered
    // floats differently (-1.0e-06 vs -1e-6). Same document, different
    // serializer — a wrong flow. raw_id=false must run the identical Rust
    // path minus the injected @id, so every numeric lexeme is compared
    // byte for byte.
    for (const key of ['dec20', 'int29', 'neg', 'small']) {
      const withMatch = withIdText.match(new RegExp(`"${key}":([^,}\\s]+)`))
      const withoutMatch = withoutIdText.match(new RegExp(`"${key}":([^,}\\s]+)`))
      expect(withMatch, key).to.not.be.null
      expect(withoutMatch, key).to.not.be.null
      expect(withoutMatch[1], key).to.equal(withMatch[1])
    }

    // 20-digit decimal and 29-digit integer survive the round trip intact.
    expect(withIdText).to.contain('"dec20":0.01234567890123456789')
    expect(withIdText).to.contain('"int29":12345678901234567890123456789')

    // raw_id=false strips only the injected @id/@type, never content.
    const withoutDoc = JSON.parse(withoutIdText)
    expect(withoutDoc).to.not.have.property('@id')
    expect(withoutDoc).to.not.have.property('@type')
    expect(Object.keys(withoutDoc).sort((a, b) => a.localeCompare(b)))
      .to.deep.equal(['dec20', 'int29', 'neg', 'small'])
  })

  it('should round extreme-precision decimals instead of producing malformed literals', async function () {
    // Why this test exists: xsd:decimal is stored with 20 fractional
    // digits, so a JSON decimal with more places is rounded at ingest.
    // Two edge cases were broken in rational_to_decimal_string:
    //
    // - 2.000...01 rounds its fraction to all zeros. The old code emitted
    //   the malformed literal "2.", which panicked the Rust store
    //   (DecimalValidationError in terminusdb-store-prolog) and returned
    //   HTTP 500. It must now round to the bare integer 2.
    // - 2.999...9 rounds up past the decimal boundary. The old code
    //   emitted "2.1" — no carry into the integer part — silently
    //   storing a wrong value. It must now carry correctly to 3.
    const insertResult = await document.insert(agent, {
      bodyString: '{"rounds_away":2.00000000000000000000001,"rounds_up":2.99999999999999999999999,"exact20":0.01234567890123456789}',
      author: 'test_author',
      queryString: 'raw_json=true&id=precision-edges&author=test_author&message=test',
    })
    const id = insertResult.body[0]

    const response = await fetch(
      `${agent.baseUrl}${api.path.document(agent)}?id=${encodeURIComponent(id)}`,
      { headers: { Authorization: util.authorizationHeader(agent) } },
    )
    expect(response.ok).to.be.true
    const text = await response.text()

    expect(text).to.contain('"rounds_away":2')
    expect(text).to.contain('"rounds_up":3')
    expect(text).to.contain('"exact20":0.01234567890123456789')
  })
})
