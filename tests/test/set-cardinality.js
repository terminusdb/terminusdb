const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')

describe('Set with Cardinality Constraints', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Set with cardinality constraints in schema', function () {
    let setCard
    let setMin
    let setMax
    let setMinMax

    before(async function () {
      setCard = util.randomString()
      setMin = util.randomString()
      setMax = util.randomString()
      setMinMax = util.randomString()

      await document
        .insert(agent, {
          schema: [
            {
              '@type': 'Class',
              '@id': setCard,
              a: {
                '@type': 'Set',
                '@class': 'xsd:integer',
                '@cardinality': 1,
              },
            },
            {
              '@type': 'Class',
              '@id': setMin,
              a: {
                '@type': 'Set',
                '@class': 'xsd:integer',
                '@min_cardinality': 1,
              },
            },
            {
              '@type': 'Class',
              '@id': setMax,
              a: {
                '@type': 'Set',
                '@class': 'xsd:integer',
                '@max_cardinality': 2,
              },
            },
            {
              '@type': 'Class',
              '@id': setMinMax,
              a: {
                '@type': 'Set',
                '@class': 'xsd:integer',
                '@min_cardinality': 1,
                '@max_cardinality': 2,
              },
            },
          ],
        })
    })

    it('responds with success for set with exact cardinality', async function () {
      await document.insert(agent, { instance: { '@type': setCard, a: [42] } })
    })

    it('fails insert under min for set with cardinality', async function () {
      for (const type of [setCard, setMin, setMinMax]) {
        const instance = { '@type': type }
        const witness = {
          '@type': 'required_field_does_not_exist_in_document',
          document: { '@type': `terminusdb:///schema#${type}` },
          field: 'terminusdb:///schema#a',
        }
        await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
      }
    })

    it('fails insert over max for set with cardinality', async function () {
      const instance = { a: [42, 23, 12] }
      const witness = {
        '@type': 'field_has_wrong_cardinality',
        actual: instance.a.length,
        document: { 'terminusdb:///schema#a': instance.a },
        field: 'terminusdb:///schema#a',
      }
      instance['@type'] = setCard
      witness.document['@type'] = `terminusdb:///schema#${setCard}`
      witness.min = 1
      witness.max = 1
      await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
      instance['@type'] = setMax
      witness.document['@type'] = `terminusdb:///schema#${setMax}`
      witness.min = 0
      witness.max = 2
      await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
      instance['@type'] = setMinMax
      witness.document['@type'] = `terminusdb:///schema#${setMinMax}`
      witness.min = 1
      witness.max = 2
      await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
    })

    it('responds with success for set with min cardinality', async function () {
      await document.insert(agent, { instance: { '@type': setMin, a: [42, 43] } })
    })

    it('responds with success for set with max cardinality', async function () {
      await document.insert(agent, { instance: { '@type': setMax, a: [42, 43] } })
    })

    it('responds with success for nothing in set with max only', async function () {
      await document.insert(agent, { instance: { '@type': setMax } })
    })

    it('responds with success for set with min and max cardinality', async function () {
      await document.insert(agent, { instance: { '@type': setMinMax, a: [42, 43] } })
    })
  })

  describe('Schema migration from Cardinality to Set with cardinality', function () {
    let cardinalityClass
    let instanceId

    before(async function () {
      cardinalityClass = util.randomString()

      // Create schema with Cardinality type
      await document.insert(agent, {
        schema: {
          '@type': 'Class',
          '@id': cardinalityClass,
          items: {
            '@type': 'Cardinality',
            '@class': 'xsd:string',
            '@min_cardinality': 1,
            '@max_cardinality': 3,
          },
        },
      })

      // Insert instance with Cardinality type
      const result = await document.insert(agent, {
        instance: {
          '@type': cardinalityClass,
          items: ['one', 'two'],
        },
      })
      instanceId = result.body[0]
    })

    it('instance exists with Cardinality schema', async function () {
      const doc = await document.get(agent, { query: { id: instanceId, as_list: true } })
      expect(doc.body[0].items).to.have.members(['one', 'two'])
    })

    it('can migrate schema from Cardinality to Set with cardinality', async function () {
      // Replace schema changing Cardinality to Set with same constraints
      await document.replace(agent, {
        schema: {
          '@type': 'Class',
          '@id': cardinalityClass,
          items: {
            '@type': 'Set',
            '@class': 'xsd:string',
            '@min_cardinality': 1,
            '@max_cardinality': 3,
          },
        },
      })

      // Instance should still exist and be valid
      const doc = await document.get(agent, { query: { id: instanceId, as_list: true } })
      expect(doc.body[0].items).to.have.members(['one', 'two'])
    })

    it('cardinality constraints still enforced after migration to Set', async function () {
      // Try to insert too many items - should fail
      const instance = {
        '@type': cardinalityClass,
        items: ['a', 'b', 'c', 'd'],
      }
      const witness = {
        '@type': 'field_has_wrong_cardinality',
        actual: 4,
        document: { 'terminusdb:///schema#items': instance.items },
        field: 'terminusdb:///schema#items',
        min: 1,
        max: 3,
      }
      witness.document['@type'] = `terminusdb:///schema#${cardinalityClass}`
      await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
    })

    it('can insert valid instance after migration', async function () {
      await document.insert(agent, {
        instance: {
          '@type': cardinalityClass,
          items: ['valid', 'instance', 'here'],
        },
      })
    })
  })

  describe('Set without cardinality constraints (backward compatibility)', function () {
    let unconstrainedSet

    before(async function () {
      unconstrainedSet = util.randomString()

      await document.insert(agent, {
        schema: {
          '@type': 'Class',
          '@id': unconstrainedSet,
          items: {
            '@type': 'Set',
            '@class': 'xsd:string',
          },
        },
      })
    })

    it('accepts empty set', async function () {
      await document.insert(agent, {
        instance: { '@type': unconstrainedSet },
      })
    })

    it('accepts any number of items', async function () {
      await document.insert(agent, {
        instance: { '@type': unconstrainedSet, items: ['a'] },
      })
      await document.insert(agent, {
        instance: { '@type': unconstrainedSet, items: ['a', 'b', 'c', 'd', 'e'] },
      })
    })
  })

  describe('Schema document includes cardinality for Set', function () {
    let schemaClass

    before(async function () {
      schemaClass = util.randomString()

      await document.insert(agent, {
        schema: {
          '@type': 'Class',
          '@id': schemaClass,
          constrained: {
            '@type': 'Set',
            '@class': 'xsd:integer',
            '@min_cardinality': 2,
            '@max_cardinality': 5,
          },
          unconstrained: {
            '@type': 'Set',
            '@class': 'xsd:integer',
          },
        },
      })
    })

    it('retrieves schema document with Set cardinality constraints', async function () {
      const result = await document.get(agent, {
        query: { graph_type: 'schema', id: schemaClass, as_list: true },
      })
      const schema = result.body[0]

      // Check constrained set has min/max cardinality
      expect(schema.constrained['@type']).to.equal('Set')
      expect(schema.constrained['@min_cardinality']).to.equal(2)
      expect(schema.constrained['@max_cardinality']).to.equal(5)

      // Check unconstrained set does not have min/max
      expect(schema.unconstrained['@type']).to.equal('Set')
      expect(schema.unconstrained).to.not.have.property('@min_cardinality')
      expect(schema.unconstrained).to.not.have.property('@max_cardinality')
    })
  })
})
