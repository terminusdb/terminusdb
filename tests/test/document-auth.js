const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')

describe('document', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('1 database, shared', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    describe('fails with missing parameter', function () {
      const options = [
        [{ queryString: '', bodyString: '' }, 'author'],
        [{ queryString: 'author=a', bodyString: '' }, 'message'],
      ]
      for (const [option, p] of options) {
        it(p, async function () {
          await document.insert(agent, option).fails(api.error.missingParameter(p))
          await document.replace(agent, option).fails(api.error.missingParameter(p))
        })
      }
    })

    it('fails replace schema for document not found', async function () {
      const schema = { '@id': util.randomString(), '@type': 'Class' }
      await document.replace(agent, { schema }).notFound(schema)
    })

    describe('fails insert schema with bad field value (#647)', function () {
      const identifiers = [
        '',
        13,
        {},
        ['b'],
        false,
        true,
        null,
      ]
      for (const id of identifiers) {
        it(JSON.stringify(id), async function () {
          {
            const schema = { '@id': id, '@type': 'Class' }
            await document.insert(agent, { schema }).fails(api.error.badFieldValue('@id', id, schema))
          }
          {
            const schema = { '@id': util.randomString(), '@type': id }
            await document.insert(agent, { schema }).fails(api.error.badFieldValue('@type', id, schema))
          }
        })
      }
    })

    describe('fails insert schema with missing field', function () {
      const schemas = [
        [{ '@id': true }, '@type'],
        [{ '@type': 'Class' }, '@id'],
      ]
      for (const [schema, field] of schemas) {
        it(JSON.stringify(schema), async function () {
          if (schema['@id']) {
            schema['@id'] = util.randomString()
          }
          await document.insert(agent, { schema }).fails(api.error.missingField(field, schema))
        })
      }
    })

    describe('inserts, queries, and deletes schema with @id', function () {
      const keys = [
        'false',
        'true',
        'null',
        '8',
        '[]',
        '{}',
        '/',
        '逆手道',
      ]
      for (const id of keys) {
        it(id, async function () {
          const schema = { '@id': id, '@type': 'Class' }
          await document.insert(agent, { schema })
          const r = await document.get(agent, { query: { graph_type: 'schema', id } })
          await document.delete(agent, { query: { graph_type: 'schema', id } })
          expect(r.body).to.deep.equal(schema)
        })
      }
    })

    it('passes insert schema, instance with expected @id values (object)', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      const instance = { '@type': id, '@id': `terminusdb:///data/${id}/0` }
      await document.insert(agent, { instance })
    })

    it('passes insert schema, instance with expected @id values (array)', async function () {
      const id1 = util.randomString()
      const id2 = util.randomString()
      const schema = [{ '@type': 'Class', '@id': id1 }, { '@type': 'Class', '@id': id2 }]
      await document.insert(agent, { schema })
      const instance = [
        { '@type': id1, '@id': `terminusdb:///data/${id1}/1` },
        { '@type': id2, '@id': `terminusdb:///data/${id2}/2` },
      ]
      await document.insert(agent, { instance })
    })

    it('passes HEAD call on existing db and with permissions', async function () {
      const systemPath = api.path.documentSystem()
      const response = await agent.head(systemPath).send()
      expect(response.headers['terminusdb-data-version']).to.match(/^system:/)
      expect(response.statusCode).to.equal(200)
    })

    it('returns 404 on HEAD call on non-existing db', async function () {
      const path = api.path.document({ orgName: 'admin', dbName: 'nonExistingDb' })
      const response = await agent.head(path).send()
      expect(response.statusCode).to.equal(404)
    })

    it('returns 400 on HEAD call with wrong data-version', async function () {
      const path = api.path.documentSystem()
      const response = await agent.head(path)
        .set('Terminusdb-Data-Version', 'system:nonMatchingDataVersion')
        .send()
      expect(response.statusCode).to.equal(400)
    })

    it('returns 200 on HEAD call with proper data-version', async function () {
      const systemPath = api.path.documentSystem()
      const response = await agent.head(systemPath).send()
      const dataVersion = response.headers['terminusdb-data-version']
      const responseWithDataVersion = await agent.head(systemPath)
        .set('Terminusdb-Data-Version', dataVersion)
        .send()
      expect(responseWithDataVersion.statusCode).to.equal(200)
    })

    it('returns 403 forbidden on HEAD call with user that does not have DB access', async function () {
      const systemPath = api.path.documentSystem()
      const userName = util.randomString()
      await agent
        .post('/api/users')
        .send({
          name: userName,
          password: userName,
        })
      const agentNewUser = new Agent().auth({ user: userName, password: userName, skipJwt: true })
      const response = await agentNewUser.head(systemPath).send()
      expect(response.statusCode).to.equal(403)
    })

    it('fails on subdocument @key checks (#566)', async function () {
      const schema = { '@type': 'Class', '@subdocument': [] }
      schema['@id'] = util.randomString()
      await document.insert(agent, { schema }).fails(api.error.subdocumentKeyMissing(schema))
      schema['@id'] = util.randomString()
      schema['@key'] = { useless_key: 'useless_value' }
      await document.insert(agent, { schema }).fails(api.error.documentKeyTypeMissing(schema))
      schema['@id'] = util.randomString()
      schema['@key'] = { '@type': 'Unknown' }
      await document.insert(agent, { schema }).fails(api.error.documentKeyTypeUnknown(schema))
    })

    it('fails when @key value is not an object (#587)', async function () {
      const schema = { '@id': util.randomString(), '@type': 'Class', '@key': false }
      await document.insert(agent, { schema }).fails(api.error.documentKeyNotObject(schema))
    })

    it('handles different @id types (#622)', async function () {
      const type1 = util.randomString()
      const type2 = util.randomString()
      const schema = [
        { '@type': 'Class', '@id': type1 },
        { '@type': 'Class', '@id': type2, ref: type1 },
      ]
      await document.insert(agent, { schema })
      {
        const instance = [
          { '@type': type1, '@id': `terminusdb:///data/${type1}/1` },
          { '@type': type2, '@id': `terminusdb:///data/${type2}/1`, ref: `terminusdb:///data/${type1}/1` },
          { '@type': type2, '@id': `terminusdb:///data/${type2}/3`, ref: { '@id': `terminusdb:///data/${type1}/1`, '@type': '@id' } },
        ]
        await document.insert(agent, { instance })
      }
      const badValue = { field: 'abc' }
      const witness = {
        'terminusdb:///schema#ref': {
          '@type': 'no_unique_type_for_document',
          document: badValue,
        },
      }
      {
        const instance = { '@type': type2, '@id': `terminusdb:///data/${type1}/4`, ref: badValue }
        await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
      }
      {
        const instance = { '@type': type2, '@id': `terminusdb:///data/${type1}/1`, ref: badValue }
        await document.replace(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
      }
    })

    it('fails insert instance with unexpected list (#623)', async function () {
      const type = util.randomString()
      const expectedType = 'http://www.w3.org/2001/XMLSchema#string'
      const schema = { '@id': type, '@type': 'Class', s: expectedType }
      await document.insert(agent, { schema })
      const badValue = ['a', 'b']
      const witness = {
        '@type': 'unexpected_list',
        type: 'http://www.w3.org/2001/XMLSchema#string',
        value: badValue,
      }
      const instance = { '@type': type, s: badValue }
      await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
    })

    it('fails insert instance with unexpected boolean value (#515)', async function () {
      const type = util.randomString()
      const expectedType = 'http://www.w3.org/2001/XMLSchema#string'
      const schema = { '@id': type, '@type': 'Class', s: expectedType }
      await document.insert(agent, { schema })
      for (const value of [false, true]) {
        const instance = { '@type': type, s: value }
        await document.insert(agent, { instance }).fails(api.error.unexpectedBooleanValue(value, expectedType))
      }
    })

    it('fails insert with invalid array dimensions (#975)', async function () {
      const type = util.randomString()
      const schema = {
        '@id': type,
        '@type': 'Class',
        s: { '@type': 'Array', '@dimensions': 1, '@class': 'xsd:integer' },
      }
      await document.insert(agent, { schema })
      const instance = { '@type': type, s: [[1], [2]] }
      const witness = { '@type': 'invalid_array_dimensions', array: instance.s, dimensions: 1 }
      await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
    })

    it('does not stringify boolean literals (#723)', async function () {
      const type = util.randomString()
      const id = type + '/' + util.randomString()
      const schema = {
        '@id': type,
        '@type': 'Class',
        bfalse: 'xsd:boolean',
        btrue: 'xsd:boolean',
      }
      await document.insert(agent, { schema })
      await document.insert(agent, { instance: { '@type': type, '@id': id, bfalse: false, btrue: true } })
      const r = await document.get(agent, { query: { id } })
      expect(r.body['@id']).to.equal(id)
      expect(r.body['@type']).to.equal(type)
      expect(r.body.bfalse).to.equal(false)
      expect(r.body.btrue).to.equal(true)
    })

    it('does not drop incoming links (#736)', async function () {
      const type1 = util.randomString()
      const type2 = util.randomString()
      const class1 = { '@type': 'Class', '@id': type1 }
      // This class has a reference to the previous class.
      const class2 = { '@type': 'Class', '@id': type2, id1: { '@type': 'Optional', '@class': type1 } }
      await document.insert(agent, { schema: [class1, class2] })
      const doc1 = { '@type': type1, '@id': type1 + '/1' }
      const doc2 = { '@type': type2, '@id': type2 + '/2', id1: doc1['@id'] }
      await document.insert(agent, { instance: [doc1, doc2] })
      await document.replace(agent, { instance: doc1 })
      const r = await document.get(agent, { body: { id: doc2['@id'] } })
      // Even though doc1 was replaced, doc2 should still have the same reference.
      expect(r.body).to.deep.equal(doc2)
    })

    it('passes replace create if instance does not exist (#705)', async function () {
      const type = util.randomString()
      const schema = {
        '@type': 'Class',
        '@id': type,
        '@key': { '@type': 'Lexical', '@fields': ['name'] },
        name: 'xsd:string',
      }
      await document.insert(agent, { schema })
      const name = type + '/1'
      const instance = { '@type': type, name }
      await document.replace(agent, { instance, create: true })
      const r = await document.get(agent, { query: { type } })
      instance['@id'] = `${type}/${type}%2F1`
      expect(r.body).to.deep.equal(instance)
    })

    describe('fails insert schema with reserved name', function () {
      const reservedIds = [
        'JSONDocument',
        'terminusdb:///schema#JSONDocument',
      ]
      for (const id of reservedIds) {
        it(id, async function () {
          const schema = { '@type': 'Class', '@id': id }
          await document.insert(agent, { schema }).fails(api.error.documentInsertionReservedName(id))
        })
      }
    })

    describe('key @fields', function () {
      const schema = { '@type': 'Class' }
      const keyTypes = [
        'Hash',
        'Lexical',
      ]

      beforeEach(function () {
        schema['@id'] = util.randomString()
      })

      for (const keyType of keyTypes) {
        it(`fails when @fields is missing for ${keyType}`, async function () {
          schema['@key'] = { '@type': keyType }
          await document.insert(agent, { schema }).fails(api.error.keyMissingFields(schema))
        })

        it(`fails when @fields value is not an array for ${keyType}`, async function () {
          schema['@key'] = { '@type': keyType, '@fields': { key: 'value' } }
          await document.insert(agent, { schema }).fails(api.error.keyFieldsNotAnArray(schema))
        })

        it(`fails when @fields value is empty array for ${keyType} (#727)`, async function () {
          schema['@key'] = { '@type': keyType, '@fields': [] }
          await document.insert(agent, { schema }).fails(api.error.keyFieldsIsEmpty(schema))
        })
      }
    })

    describe('Deep replace/insertions', function () {
      let A
      let B
      let schema
      before(async function () {
        A = util.randomString()
        B = util.randomString()
        schema = [
          {
            '@type': 'Class',
            '@id': A,
            b: B,
          },
          {
            '@type': 'Class',
            '@unfoldable': [],
            '@id': B,
            x: 'xsd:string',
          }]
        await document.insert(agent, { schema })
      })

      it('add in parallel', async function () {
        const instance = [
          { b: { '@id': `${B}/1`, x: 'asdf' } },
          { b: { '@id': `${B}/1`, x: 'fdsa' } },
        ]
        const result = await document.insert(agent, { instance }).unverified()
        expect(result.body['api:error']['@type']).to.equal('api:SameDocumentIdsMutatedInOneTransaction')
        expect(result.body['api:error']['api:duplicate_ids']).to.deep.equal([`terminusdb:///data/${B}/1`])
      })

      it('deep replace', async function () {
        const instance = [
          {
            '@id': `${A}/2`,
            b: {
              '@id': `${B}/2`,
              x: 'asdf',
            },
          },
        ]
        await document.insert(agent, { instance }).unverified()
        const instance2 = [
          {
            '@id': `${A}/2`,
            b: {
              '@id': `${B}/2`,
              x: 'fdsa',
            },
          },
        ]
        await document.replace(agent, { instance: instance2 })
        const result = await document.get(agent, { query: { id: `${A}/2` } })
        expect(result.body.b.x).to.equal('fdsa')
      })
    })

    it('succeeds when ignoring optional combined with oneof (#992)', async function () {
      const Parent = util.randomString()
      const Choice = util.randomString()
      const Container = util.randomString()
      const schema = [
        {
          '@type': 'Class',
          '@id': Parent,
          optional: { '@type': 'Optional', '@class': 'xsd:integer' },
        },
        {
          '@type': 'TaggedUnion',
          '@id': Choice,
          '@key': { '@type': 'ValueHash' },
          '@inherits': [Parent],
          '@subdocument': [],
          integer: 'xsd:integer',
          boolean: 'xsd:boolean',
        },
        {
          '@type': 'Class',
          '@id': Container,
          contains: { '@type': 'Set', '@class': Parent },
        },
      ]
      await document.insert(agent, { schema })

      const instance = {
        '@type': Container,
        contains: [
          {
            '@type': Choice,
            integer: 12,
          },
        ],
      }
      await document.insert(agent, { instance })
    })

    it('passes insert schema with subdocument and @documentation (#670)', async function () {
      const schema = {
        '@id': util.randomString(),
        '@type': 'Class',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        '@documentation': {
          '@comment': 'A random subdocument number?',
          '@properties': { n: 'A number!' },
        },
        n: 'xsd:integer',
      }
      await document.insert(agent, { schema })
      const r = await document.get(agent, { query: { graph_type: 'schema', id: schema['@id'] } })
      expect(r.body).to.deep.equal(schema)
    })

    it('passes insert schema with no @comment in @documentation (#1041)', async function () {
      const schema = [
        {
          '@id': util.randomString(),
          '@type': 'Class',
          '@documentation': { '@properties': { n: 'A number!' } },
          n: 'xsd:integer',
        },
        {
          '@id': util.randomString(),
          '@type': 'Enum',
          '@documentation': { '@values': { i: 'An item?' } },
          '@value': ['i'],
        },
      ]
      await document.insert(agent, { schema })
      const r = await document.get(agent, { query: { graph_type: 'schema', id: schema['@id'], as_list: true } })
      expect(r.body).to.deep.include.members(schema)
    })

    it('fails insert instance with unknown property (#1030)', async function () {
      const schema = { '@id': util.randomString(), '@type': 'Class' }
      await document.insert(agent, { schema })
      const instance = { '@type': schema['@id'], unknown: 'abc' }
      const type = `terminusdb:///schema#${schema['@id']}`
      const witness = {
        '@type': 'unknown_property_for_type',
        document: { '@type': type, 'terminusdb:///schema#unknown': 'abc' },
        property: ['terminusdb:///schema#unknown'],
        type,
      }
      await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
    })

    describe('tests cardinality in schema', function () {
      let card
      let min
      let max
      let minmax

      before(async function () {
        card = util.randomString()
        min = util.randomString()
        max = util.randomString()
        minmax = util.randomString()

        await document
          .insert(agent, {
            schema: [
              {
                '@type': 'Class',
                '@id': card,
                a: {
                  '@type': 'Cardinality',
                  '@class': 'xsd:integer',
                  '@cardinality': 1,
                },
              },
              {
                '@type': 'Class',
                '@id': min,
                a: {
                  '@type': 'Cardinality',
                  '@class': 'xsd:integer',
                  '@min_cardinality': 1,
                },
              },
              {
                '@type': 'Class',
                '@id': max,
                a: {
                  '@type': 'Cardinality',
                  '@class': 'xsd:integer',
                  '@max_cardinality': 2,
                },
              },
              {
                '@type': 'Class',
                '@id': minmax,
                a: {
                  '@type': 'Cardinality',
                  '@class': 'xsd:integer',
                  '@min_cardinality': 1,
                  '@max_cardinality': 2,
                },
              },
            ],
          })
      })

      it('responds with success for card', async function () {
        await document.insert(agent, { instance: { '@type': card, a: [42] } })
      })

      it('fails insert under min', async function () {
        for (const type of [card, min, minmax]) {
          const instance = { '@type': type }
          const witness = {
            '@type': 'required_field_does_not_exist_in_document',
            document: { '@type': `terminusdb:///schema#${type}` },
            field: 'terminusdb:///schema#a',
          }
          await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
        }
      })

      it('fails insert over max', async function () {
        const instance = { a: [42, 23, 12] }
        const witness = {
          '@type': 'field_has_wrong_cardinality',
          actual: instance.a.length,
          document: { 'terminusdb:///schema#a': instance.a },
          field: 'terminusdb:///schema#a',
        }
        instance['@type'] = card
        witness.document['@type'] = `terminusdb:///schema#${card}`
        witness.min = 1
        witness.max = 1
        await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
        instance['@type'] = max
        witness.document['@type'] = `terminusdb:///schema#${max}`
        witness.min = 0
        witness.max = 2
        await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
        instance['@type'] = minmax
        witness.document['@type'] = `terminusdb:///schema#${minmax}`
        witness.min = 1
        witness.max = 2
        await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
      })

      it('responds with success for min', async function () {
        await document.insert(agent, { instance: { '@type': min, a: [42, 43] } })
      })

      it('responds with success for max', async function () {
        await document.insert(agent, { instance: { '@type': max, a: [42, 43] } })
      })

      it('responds with success for nothing in max', async function () {
        await document.insert(agent, { instance: { '@type': max } })
      })

      it('responds with success for minmax', async function () {
        await document.insert(agent, { instance: { '@type': minmax, a: [42, 43] } })
      })

      it('passes insert, get JSONDocument', async function () {
        const r1 = await document.insert(agent, { instance: { a: [42, 23, 12] }, raw_json: true })
        const id = r1.body[0]
        const r2 = await document.get(agent, { query: { id, as_list: true, compress_ids: false } })
        expect(r2.body).to.deep.equal([{ '@id': id, a: ['42', '23', '12'] }])
      })

      it('fails insert with invalid JSONDocument @id', async function () {
        const instance = { '@id': util.randomString(), a: [42, 23, 12] }
        const id = `terminusdb:///data/${instance['@id']}`
        await document.insert(agent, { instance, raw_json: true }).fails(api.error.invalidJSONDocumentId(id))
      })

      it('fails gracefully with bad key prefix', async function () {
        const classid = `foo:${util.randomString()}`
        const instance = { '@type': classid }
        const result = await document.insert(agent, { instance }).unverified()
        expect(result.status).to.equal(400)
        expect(result.body['api:error']['api:key']).to.equal(classid)
      })

      it('fails with JSONDocument', async function () {
        const schema = {
          '@type': 'Class',
          '@id': util.randomString(),
          json_document: 'sys:JSONDocument',
        }
        const result = await document.insert(agent, { schema }).unverified()
        expect(result.status).to.equal(400)
        expect(result.body['api:error']['@type']).to.equal('api:JSONDocumentInvalidRangeError')
        expect(result.body['api:error']['api:field']).to.equal('json_document')
      })

      it('fails duplicate language schema', async function () {
        const schema = {
          '@type': 'Class',
          '@id': util.randomString(),
          '@documentation': [{
            '@language': 'en',
            '@label': 'Example Class',
            '@comment': 'This is an example class',
            '@properties': { name: 'name' },
          }, {
            '@language': 'en',
            '@label': 'Παράδειγμα τάξης',
            '@comment': 'Αυτό είναι ένα παράδειγμα κλάσης',
            '@properties': { name: 'όνομα' },
          }],
          name: 'xsd:string',
        }
        const r = await document.insert(agent, { schema }).unverified()
        expect(r.status).to.equal(400)
        expect(r.body['api:error']['@type']).to.equal('api:LanguageTagsRepeated')
        expect(r.body['api:error']['api:languages']).to.deep.equal(['en'])
      })

      it('fails no language schema', async function () {
        const schema = {
          '@type': 'Class',
          '@id': util.randomString(),
          '@documentation': [{
            '@label': 'Example Class',
            '@comment': 'This is an example class',
            '@properties': { name: 'name' },
          }, {
            '@language': 'en',
            '@label': 'Παράδειγμα τάξης',
            '@comment': 'Αυτό είναι ένα παράδειγμα κλάσης',
            '@properties': { name: 'όνομα' },
          }],
          name: 'xsd:string',
        }
        const r = await document.insert(agent, { schema }).unverified()
        expect(r.status).to.equal(400)
        expect(r.body['api:error']['@type']).to.equal('api:NoLanguageTagForMultilingual')
      })

      it('saves metadata', async function () {
        const schema = {
          '@type': 'Class',
          '@id': util.randomString(),
          '@metadata': { some: 'metadata' },
          name: 'xsd:string',
        }
        await document.insert(agent, { schema }).unverified()
        const r = await document.get(agent, { query: { graph_type: 'schema', id: schema['@id'] } }).unverified()
        expect(r.body['@metadata']).to.deep.equal({ some: 'metadata' })
      })

      it('fails bad enum', async function () {
        const schema = {
          '@type': 'Enum',
          '@id': util.randomString(),
        }
        const r = await document.insert(agent, { schema }).unverified()
        expect(r.status).to.equal(400)
        expect(r.body['api:error']['@type']).to.equal('api:InvalidEnumValues')
      })
    })

    it('fails with 404 for nonexistent schema document', async function () {
      const r = await document.get(agent, { query: { graph_type: 'schema', id: 'asdf' } }).unverified()
      expect(r.status).to.equal(404)
      expect(r.body['api:error']['@type']).to.equal('api:DocumentNotFound')
    })

    it('fails with 404 for nonexistent instance document', async function () {
      const r = await document.get(agent, { query: { id: 'asdf' } }).unverified()
      expect(r.status).to.equal(404)
      expect(r.body['api:error']['@type']).to.equal('api:DocumentNotFound')
    })
  })
})
