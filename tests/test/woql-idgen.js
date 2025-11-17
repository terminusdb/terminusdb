const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('woql-idgen', function () {
  let agent

  beforeEach(async function () {
    agent = new Agent().auth()
    agent.dbName = `test_idgen_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`
    await db.create(agent)
  })

  afterEach(async function () {
    await db.delete(agent)
  })

  describe('LexicalKey', function () {
    it('generates deterministic IDs from key list', async function () {
      const query = {
        '@type': 'LexicalKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/Person/' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Jane' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': '1990-05-15' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const result = await woql.post(agent, query)
      const bindings = result.body.bindings

      expect(bindings).to.have.length(1)
      expect(bindings[0].ID).to.exist
      expect(bindings[0].ID).to.include('Person/')
      expect(bindings[0].ID).to.include('Jane')
      expect(bindings[0].ID).to.include('1990-05-15')
    })

    it('generates same ID for same inputs', async function () {
      const query = {
        '@type': 'LexicalKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/Person/' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'John' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Doe' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const result1 = await woql.post(agent, query)
      const result2 = await woql.post(agent, query)

      const id1 = result1.body.bindings[0].ID
      const id2 = result2.body.bindings[0].ID

      expect(id1).to.equal(id2)
    })

    it('generates different IDs for different inputs', async function () {
      const query1 = {
        '@type': 'LexicalKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/Person/' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Alice' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const query2 = {
        '@type': 'LexicalKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/Person/' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Bob' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const result1 = await woql.post(agent, query1)
      const result2 = await woql.post(agent, query2)

      const id1 = result1.body.bindings[0].ID
      const id2 = result2.body.bindings[0].ID

      expect(id1).to.not.equal(id2)
    })

    it('can be used with variables in complex queries', async function () {
      // Test that LexicalKey can be used alongside other WOQL operations
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'LexicalKey',
            base: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'Person/' },
            },
            key_list: {
              '@type': 'DataValue',
              list: [
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Sarah' } },
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': '1990-01-01' } },
              ],
            },
            uri: { '@type': 'NodeValue', variable: 'ID1' },
          },
          {
            '@type': 'LexicalKey',
            base: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'Person/' },
            },
            key_list: {
              '@type': 'DataValue',
              list: [
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'John' } },
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': '1985-05-15' } },
              ],
            },
            uri: { '@type': 'NodeValue', variable: 'ID2' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      const bindings = result.body.bindings

      expect(bindings).to.have.length(1)
      expect(bindings[0].ID1).to.include('Sarah')
      expect(bindings[0].ID2).to.include('John')
      expect(bindings[0].ID1).to.not.equal(bindings[0].ID2)
    })

    it('fails with missing base', async function () {
      const query = {
        '@type': 'LexicalKey',
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'test' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.body['api:error']).to.have.property('@type').that.includes('Error')
    })

    it('fails with missing key_list', async function () {
      const query = {
        '@type': 'LexicalKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/test_' },
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.body['api:error']).to.have.property('@type').that.includes('Error')
    })

    it('fails with missing uri', async function () {
      const query = {
        '@type': 'LexicalKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/test_' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'test' } },
          ],
        },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.body['api:error']).to.have.property('@type').that.includes('Error')
    })
  })

  describe('HashKey', function () {
    it('generates deterministic hash IDs from key list', async function () {
      const query = {
        '@type': 'HashKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/Employee/' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Jane' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'E12345' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const result = await woql.post(agent, query)
      const bindings = result.body.bindings

      expect(bindings).to.have.length(1)
      expect(bindings[0].ID).to.exist
      expect(bindings[0].ID).to.include('Employee/')

      // Hash should be a hex string (64 chars for SHA-256)
      const hashPart = bindings[0].ID.split('Employee/')[1]
      expect(hashPart).to.match(/^[a-f0-9]{64}$/)
    })

    it('generates same hash ID for same inputs', async function () {
      const query = {
        '@type': 'HashKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/Employee/' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'John' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'E99999' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const result1 = await woql.post(agent, query)
      const result2 = await woql.post(agent, query)

      const id1 = result1.body.bindings[0].ID
      const id2 = result2.body.bindings[0].ID

      expect(id1).to.equal(id2)
    })

    it('generates different hash IDs for different inputs', async function () {
      const query1 = {
        '@type': 'HashKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/Employee/' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Alice' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const query2 = {
        '@type': 'HashKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/Employee/' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Bob' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const result1 = await woql.post(agent, query1)
      const result2 = await woql.post(agent, query2)

      const id1 = result1.body.bindings[0].ID
      const id2 = result2.body.bindings[0].ID

      expect(id1).to.not.equal(id2)
    })

    it('can be used with variables in complex queries', async function () {
      // Test that HashKey can be used alongside other WOQL operations
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'HashKey',
            base: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'Employee/' },
            },
            key_list: {
              '@type': 'DataValue',
              list: [
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'E12345' } },
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Jane' } },
              ],
            },
            uri: { '@type': 'NodeValue', variable: 'ID1' },
          },
          {
            '@type': 'HashKey',
            base: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'Employee/' },
            },
            key_list: {
              '@type': 'DataValue',
              list: [
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'E99999' } },
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'John' } },
              ],
            },
            uri: { '@type': 'NodeValue', variable: 'ID2' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      const bindings = result.body.bindings

      expect(bindings).to.have.length(1)
      expect(bindings[0].ID1).to.include('Employee/')
      expect(bindings[0].ID2).to.include('Employee/')
      expect(bindings[0].ID1).to.not.equal(bindings[0].ID2)

      // Both should be valid SHA-256 hashes
      const hash1 = bindings[0].ID1.split('Employee/')[1]
      const hash2 = bindings[0].ID2.split('Employee/')[1]
      expect(hash1).to.match(/^[a-f0-9]{64}$/)
      expect(hash2).to.match(/^[a-f0-9]{64}$/)
    })

    it('fails with missing base', async function () {
      const query = {
        '@type': 'HashKey',
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'test' } },
          ],
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.body['api:error']).to.have.property('@type').that.includes('Error')
    })

    it('fails with missing key_list', async function () {
      const query = {
        '@type': 'HashKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/test_' },
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.body['api:error']).to.have.property('@type').that.includes('Error')
    })

    it('fails with missing uri', async function () {
      const query = {
        '@type': 'HashKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/test_' },
        },
        key_list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'test' } },
          ],
        },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.body['api:error']).to.have.property('@type').that.includes('Error')
    })
  })

  describe('RandomKey', function () {
    it('generates unique random IDs', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'RandomKey',
            base: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/random_' },
            },
            uri: { '@type': 'NodeValue', variable: 'ID1' },
          },
          {
            '@type': 'RandomKey',
            base: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/random_' },
            },
            uri: { '@type': 'NodeValue', variable: 'ID2' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      const bindings = result.body.bindings

      // Should have one binding with two different IDs
      expect(bindings).to.have.length(1)
      expect(bindings[0].ID1).to.exist
      expect(bindings[0].ID2).to.exist

      // IDs should be different
      expect(bindings[0].ID1).to.not.equal(bindings[0].ID2)

      // Both should start with the base
      expect(bindings[0].ID1).to.include('random_')
      expect(bindings[0].ID2).to.include('random_')

      // Both should have random suffixes (16 base64 chars)
      const id1Suffix = bindings[0].ID1.split('random_')[1]
      const id2Suffix = bindings[0].ID2.split('random_')[1]
      expect(id1Suffix).to.have.length(16)
      expect(id2Suffix).to.have.length(16)
    })

    it('generates different IDs on multiple executions', async function () {
      const query = {
        '@type': 'RandomKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/test_' },
        },
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const ids = new Set()

      // Run the query 10 times
      for (let i = 0; i < 10; i++) {
        const result = await woql.post(agent, query)
        const id = result.body.bindings[0].ID
        ids.add(id)
      }

      // All 10 IDs should be unique
      expect(ids.size).to.equal(10)
    })

    it('can be used with variables in complex queries', async function () {
      // Test that RandomKey can be used alongside other WOQL operations
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'RandomKey',
            base: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'Event/' },
            },
            uri: { '@type': 'NodeValue', variable: 'ID1' },
          },
          {
            '@type': 'RandomKey',
            base: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'Event/' },
            },
            uri: { '@type': 'NodeValue', variable: 'ID2' },
          },
          {
            '@type': 'RandomKey',
            base: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'Event/' },
            },
            uri: { '@type': 'NodeValue', variable: 'ID3' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      const bindings = result.body.bindings

      expect(bindings).to.have.length(1)
      expect(bindings[0].ID1).to.include('Event/')
      expect(bindings[0].ID2).to.include('Event/')
      expect(bindings[0].ID3).to.include('Event/')

      // All three IDs should be different (random)
      expect(bindings[0].ID1).to.not.equal(bindings[0].ID2)
      expect(bindings[0].ID2).to.not.equal(bindings[0].ID3)
      expect(bindings[0].ID1).to.not.equal(bindings[0].ID3)

      // All should have 16-character random suffixes
      const suffix1 = bindings[0].ID1.split('Event/')[1]
      const suffix2 = bindings[0].ID2.split('Event/')[1]
      const suffix3 = bindings[0].ID3.split('Event/')[1]
      expect(suffix1).to.have.length(16)
      expect(suffix2).to.have.length(16)
      expect(suffix3).to.have.length(16)
    })

    it('fails with missing base', async function () {
      const query = {
        '@type': 'RandomKey',
        uri: { '@type': 'NodeValue', variable: 'ID' },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.body['api:error']).to.have.property('@type').that.includes('Error')
    })

    it('fails with missing uri', async function () {
      const query = {
        '@type': 'RandomKey',
        base: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:string', '@value': 'terminusdb:///data/test_' },
        },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.body['api:error']).to.have.property('@type').that.includes('Error')
    })
  })
})
