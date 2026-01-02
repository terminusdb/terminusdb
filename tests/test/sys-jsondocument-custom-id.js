const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

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
})
