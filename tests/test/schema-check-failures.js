const { Agent, api, db, document, util } = require('../lib')

describe('schema-check-failures', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('fails replace schema with new non-optional field (#780)', async function () {
    // Insert an initial schema.
    const schema = { '@id': util.randomString(), '@type': 'Class' }
    await document.insert(agent, { schema })
    // Insert an initial instance.
    const instance = { '@type': schema['@id'], '@id': `${schema['@id']}/0` }
    await document.insert(agent, { instance })
    // Update the schema with a new field that is not Optional.
    schema.name = 'xsd:string'
    const witness = {
      '@type': 'instance_not_cardinality_one',
      class: 'http://www.w3.org/2001/XMLSchema#string',
      instance: `terminusdb:///data/${instance['@id']}`,
      predicate: 'terminusdb:///schema#name',
    }
    await document.replace(agent, { schema }).fails((api.error.schemaCheckFailure([witness])))
  })

  it('fails insert schema with unknown class in field', async function () {
    const schema = {
      '@id': util.randomString(),
      '@type': 'Class',
      field: util.randomString(),
    }
    const witness = {
      '@type': 'not_a_class_or_base_type',
      class: `terminusdb:///schema#${schema.field}`,
    }
    await document.insert(agent, { schema }).fails((api.error.schemaCheckFailure([witness])))
  })

  describe('fails insert schema for type family with unknown @class (#1019)', function () {
    let schema
    const types = [
      'Array',
      'Cardinality',
      'List',
      'Optional',
      'Set',
      'Table',
    ]

    before(function () {
      schema = {
        '@id': util.randomString(),
        '@type': 'Class',
        field: {
          '@class': util.randomString(),
          // The '@cardinality' field is here to avoid triggering a different
          // error when the '@type' is 'Cardinality'.
          '@cardinality': 2,
        },
      }
    })

    for (const type of types) {
      it(type, async function () {
        schema.field['@type'] = type
        const witness = {
          '@type': 'not_a_class_or_base_type',
          class: `terminusdb:///schema#${schema.field['@class']}`,
        }
        await document.insert(agent, { schema }).fails((api.error.schemaCheckFailure([witness])))
      })
    }
  })

  it('fails insert instance with two @oneOf fields', async function () {
    const schema = {
      '@id': util.randomString(),
      '@type': 'Class',
      '@oneOf': { integer: 'xsd:integer', string: 'xsd:string' },
    }
    await document.insert(agent, { schema })
    {
      const instance = [
        {
          '@id': schema['@id'] + '/' + util.randomString(),
          '@type': schema['@id'],
          integer: 5,
        },
        {
          '@id': schema['@id'] + '/' + util.randomString(),
          '@type': schema['@id'],
          string: util.randomString(),
        },
      ]
      await document.insert(agent, { instance })
    }
    {
      const instance = {
        '@id': schema['@id'] + '/' + util.randomString(),
        '@type': schema['@id'],
        integer: -29,
        string: util.randomString(),
      }
      const witness = {
        '@type': 'choice_has_too_many_answers',
        choice: {
          'terminusdb:///schema#integer': 'http://www.w3.org/2001/XMLSchema#integer',
          'terminusdb:///schema#string': 'http://www.w3.org/2001/XMLSchema#string',
        },
        document: {
          '@id': `terminusdb:///data/${instance['@id']}`,
          '@type': `terminusdb:///schema#${schema['@id']}`,
          'terminusdb:///schema#integer': instance.integer,
          'terminusdb:///schema#string': instance.string,
        },
      }
      await document.insert(agent, { instance }).fails((api.error.schemaCheckFailure([witness])))
    }
  })
})
