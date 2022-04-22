const { expect } = require('chai')
const { Agent, db, document, endpoint, util } = require('../lib')

describe('document', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('1 database, shared', function () {
    let dbPath
    let docPath

    before(async function () {
      const dbDefaults = endpoint.db(agent.defaults())
      dbPath = dbDefaults.path
      docPath = endpoint.document(dbDefaults).path

      await db.createAfterDel(agent, dbPath)
    })

    after(async function () {
      await db.del(agent, dbPath)
    })

    it('succeeds on database exists', async function () {
      const r = await agent
        .head(dbPath)
        .query({ exists: true })
      expect(r.status).to.equal(200)
      expect(r.text).to.be.undefined
    })

    describe('fails insert with missing parameters', function () {
      const options = [
        [{ queryString: '', bodyString: '' }, 'author'],
        [{ queryString: 'author=a', bodyString: '' }, 'message'],
      ]
      for (const [option, missingParam] of options) {
        it(missingParam, async function () {
          const r = await document
            .insert(agent, docPath, option)
            .then(document.verifyInsertFailure)
          document.expectMissingParameter(r, missingParam)
        })
      }
    })

    describe('fails replace with missing parameters', function () {
      const options = [
        [{ queryString: '', bodyString: '' }, 'author'],
        [{ queryString: 'author=a', bodyString: '' }, 'message'],
      ]
      for (const [option, missingParam] of options) {
        it(missingParam, async function () {
          const r = await document
            .replace(agent, docPath, option)
            .then(document.verifyReplaceFailure)
          document.expectMissingParameter(r, missingParam)
        })
      }
    })

    it('fails replace for document not found', async function () {
      const instance = { '@id': util.randomString() }
      const r = await document
        .replace(agent, docPath, { instance })
        .then(document.verifyReplaceNotFound)

      expect(r.body['api:error']['@type']).to.equal('api:DocumentNotFound')
      expect(r.body['api:error']['api:document_id']).to.equal('terminusdb:///data/' + instance['@id'])
      expect(r.body['api:error']['api:document']).to.deep.equal(instance)
    })

    describe('fails on bad schema @id (#647)', function () {
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
          const schema = { '@id': id, '@type': 'Class' }
          const r = await document
            .insert(agent, docPath, { schema })
            .then(document.verifyInsertFailure)
          expect(r.body['api:error']['@type']).to.equal('api:BadFieldValue')
          expect(r.body['api:error']['api:field']).to.equal('@id')
          expect(r.body['api:error']['api:value']).to.deep.equal(id)
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
        })
      }
    })

    describe('fails on missing fields in schema', function () {
      const schemas = [
        [{ '@id': true }, '@type'],
        [{ '@type': 'Class' }, '@id'],
      ]
      for (const [schema, missingField] of schemas) {
        it(JSON.stringify(schema), async function () {
          if (schema['@id']) {
            schema['@id'] = util.randomString()
          }
          const r = await document
            .insert(agent, docPath, { schema })
            .then(document.verifyInsertFailure)
          document.expectMissingField(r, missingField, schema)
        })
      }
    })

    describe('fails on bad schema @type', function () {
      const types = [
        '',
        13,
        {},
        ['b'],
        false,
        true,
        null,
      ]
      for (const type of types) {
        it(JSON.stringify(type), async function () {
          const schema = { '@id': util.randomString(), '@type': type }
          const r = await document
            .insert(agent, docPath, { schema })
            .then(document.verifyInsertFailure)
          expect(r.body['api:error']['@type']).to.equal('api:BadFieldValue')
          expect(r.body['api:error']['api:field']).to.equal('@type')
          expect(r.body['api:error']['api:value']).to.deep.equal(type)
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
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
          await document
            .insert(agent, docPath, { schema })
            .then(document.verifyInsertSuccess)
          const r = await document
            .get(agent, docPath, { query: { graph_type: 'schema', id } })
            .then(document.verifyGetSuccess)
          await document
            .del(agent, docPath, { query: { graph_type: 'schema', id } })
            .then(document.verifyDelSuccess)
          expect(r.body).to.deep.equal(schema)
        })
      }
    })

    it('responds with expected @id values (object)', async function () {
      const id = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: { '@type': 'Class', '@id': id },
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: { '@type': id, '@id': `terminusdb:///data/${id}/0` },
        })
        .then(document.verifyInsertSuccess)
    })

    it('responds with expected @id values (array)', async function () {
      const id1 = util.randomString()
      const id2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            { '@type': 'Class', '@id': id1 },
            { '@type': 'Class', '@id': id2 },
          ],
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: [
            { '@type': id1, '@id': `terminusdb:///data/${id1}/1` },
            { '@type': id2, '@id': `terminusdb:///data/${id2}/2` },
          ],
        })
        .then(document.verifyInsertSuccess)
    })

    it('fails on subdocument @key checks (#566)', async function () {
      const schema = { '@type': 'Class', '@subdocument': [] }
      {
        schema['@id'] = util.randomString()
        const r = await document
          .insert(agent, docPath, { schema })
          .then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:SubdocumentKeyMissing')
        expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
      }
      {
        schema['@id'] = util.randomString()
        schema['@key'] = { useless_key: 'useless_value' }
        const r = await document
          .insert(agent, docPath, { schema })
          .then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:DocumentKeyTypeMissing')
        expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
        expect(JSON.parse(r.body['api:error']['api:key'])).to.deep.equal(schema['@key'])
      }
      {
        schema['@id'] = util.randomString()
        schema['@key'] = { '@type': 'Unknown' }
        const r = await document
          .insert(agent, docPath, { schema })
          .then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:DocumentKeyTypeUnknown')
        expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
        expect(r.body['api:error']['api:key_type']).to.equal(schema['@key']['@type'])
      }
    })

    it('fails when @key value is not an object (#587)', async function () {
      const schema = { '@id': util.randomString(), '@type': 'Class', '@key': false }
      const r = await document
        .insert(agent, docPath, { schema })
        .then(document.verifyInsertFailure)
      expect(r.body['api:error']['@type']).to.equal('api:DocumentKeyNotObject')
      expect(r.body['api:error']['api:key_value']).to.equal(false)
      expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
      expect(r.body['api:error']['api:document']['@key']).to.equal(false)
      expect(r.body['api:error']['api:document']['@type']).to.equal('Class')
    })

    it('handles different @id types (#622)', async function () {
      const type1 = util.randomString()
      const type2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            { '@type': 'Class', '@id': type1 },
            { '@type': 'Class', '@id': type2, ref: type1 },
          ],
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: [
            {
              '@type': type1,
              '@id': `terminusdb:///data/${type1}/1`,
            },
            {
              '@type': type2,
              '@id': `terminusdb:///data/${type2}/1`,
              ref: `terminusdb:///data/${type1}/1`,
            },
            {
              '@type': type2,
              '@id': `terminusdb:///data/${type2}/2`,
              ref: { '@id': `terminusdb:///data/${type1}/1` },
            },
            {
              '@type': type2,
              '@id': `terminusdb:///data/${type2}/3`,
              ref: { '@id': `terminusdb:///data/${type1}/1`, '@type': '@id' },
            },
          ],
        })
        .then(document.verifyInsertSuccess)
      const badValue = { field: 'abc' }
      {
        const r = await document
          .insert(agent, docPath, {
            instance: {
              '@type': type2,
              '@id': `terminusdb:///data/${type1}/4`,
              ref: badValue,
            },
          })
          .then(document.verifyInsertFailure)
        document.expectMissingField(r, '@type', badValue)
      }
      {
        const r = await document
          .replace(agent, docPath, {
            instance: {
              '@type': type2,
              '@id': `terminusdb:///data/${type1}/1`,
              ref: badValue,
            },
          })
          .then(document.verifyReplaceFailure)
        document.expectMissingField(r, '@type', badValue)
      }
    })

    it('fails for uexpected array value (#623)', async function () {
      const type = util.randomString()
      const expectedType = 'http://www.w3.org/2001/XMLSchema#string'
      await document
        .insert(agent, docPath, {
          schema: { '@id': type, '@type': 'Class', s: expectedType },
        })
        .then(document.verifyInsertSuccess)
      const badValue = ['a', 'b']
      const r = await document
        .insert(agent, docPath, {
          instance: { '@type': type, s: badValue },
        })
        .then(document.verifyInsertFailure)
      expect(r.body['api:error']['@type']).to.equal('api:UnexpectedArrayValue')
      expect(r.body['api:error']['api:value']).to.deep.equal(badValue)
      expect(r.body['api:error']['api:expected_type']).to.equal(expectedType)
    })

    it('fails for unexpected boolean values (#515)', async function () {
      const type = util.randomString()
      const expectedType = 'http://www.w3.org/2001/XMLSchema#string'
      await document
        .insert(agent, docPath, {
          schema: { '@id': type, '@type': 'Class', s: expectedType },
        })
        .then(document.verifyInsertSuccess)
      for (const value of [false, true]) {
        const r = await document
          .insert(agent, docPath, {
            instance: { '@type': type, s: value },
          })
          .then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:UnexpectedBooleanValue')
        expect(r.body['api:error']['api:value']).to.equal(value)
        expect(r.body['api:error']['api:expected_type']).to.equal(expectedType)
      }
    })

    it('fails for wrong array dimension (#975)', async function () {
      const type = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: {
            '@id': type,
            '@type': 'Class',
            s: {
              '@type': 'Array',
              '@dimensions': 1,
              '@class': 'xsd:integer',
            },
          },
        })
        .then(document.verifyInsertSuccess)
      const r = await document
        .insert(agent, docPath, {
          instance: { '@type': type, s: [[1], [2]] },
        })
        .then(document.verifyInsertFailure)
      expect(r.body['api:error']['@type']).to.equal('api:DocumentArrayWrongDimensions')
      expect(r.body['api:error']['api:dimensions']).to.equal(1)
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
      await document
        .insert(agent, docPath, {
          schema,
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: { '@type': type, '@id': id, bfalse: false, btrue: true },
        })
        .then(document.verifyInsertSuccess)
      const r = await document
        .get(agent, docPath, { query: { id } })
        .then(document.verifyGetSuccess)
      expect(r.body['@id']).to.equal(id)
      expect(r.body['@type']).to.equal(type)
      expect(r.body.bfalse).to.equal(false)
      expect(r.body.btrue).to.equal(true)
    })

    it('does not drop incoming links (#736)', async function () {
      const type1 = util.randomString()
      const type2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            {
              '@type': 'Class',
              '@id': type1,
            },
            {
              '@type': 'Class',
              '@id': type2,
              // This class has a reference to the previous class.
              id1: { '@type': 'Optional', '@class': type1 },
            },
          ],
        })
        .then(document.verifyInsertSuccess)
      const doc1 = { '@type': type1, '@id': type1 + '/1' }
      const doc2 = { '@type': type2, '@id': type2 + '/2', id1: doc1['@id'] }
      await document
        .insert(agent, docPath, { instance: [doc1, doc2] })
        .then(document.verifyInsertSuccess)
      await document
        .replace(agent, docPath, { instance: doc1 })
        .then(document.verifyReplaceSuccess)
      const r = await document
        .get(agent, docPath, { body: { id: doc2['@id'] } })
        .then(document.verifyGetSuccess)
      // Even though doc1 was replaced, doc2 should still have the same reference.
      expect(r.body).to.deep.equal(doc2)
    })

    it('it inserts if does not exist (#705)', async function () {
      const type1 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            {
              '@type': 'Class',
              '@id': type1,
              '@key': {
                '@type': 'Lexical',
                '@fields': ['name'],
              },
              name: 'xsd:string',
            },
          ],
        })
        .then(document.verifyInsertSuccess)
      const doc1 = { '@type': type1, name: type1 + '/1' }
      await document
        .replace(agent, docPath, {
          instance: [
            doc1,
          ],
          create: true,
        })
        .then(document.verifyReplaceSuccess)
      const r = await document
        .get(agent, docPath, { query: { type: type1 } })
        .then(document.verifyGetSuccess)
      // We have inserted doc1
      expect(r.body.name === type1 + '/1')
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
          const r = await document.insert(agent, docPath, { schema })
          document.verifyInsertFailure(r)
          expect(r.body['api:error']['@type']).to.equal('api:KeyMissingFields')
          expect(r.body['api:error']['api:key_type']).to.equal(keyType)
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
        })

        it(`fails when @fields value is not an array for ${keyType}`, async function () {
          schema['@key'] = { '@type': keyType, '@fields': { key: 'value' } }
          const r = await document.insert(agent, docPath, { schema })
          document.verifyInsertFailure(r)
          expect(r.body['api:error']['@type']).to.equal('api:KeyFieldsNotAnArray')
          expect(r.body['api:error']['api:fields']).to.deep.equal(schema['@key']['@fields'])
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
        })

        it(`fails when @fields value is empty array for ${keyType} (#727)`, async function () {
          schema['@key'] = { '@type': keyType, '@fields': [] }
          const r = await document.insert(agent, docPath, { schema })
          document.verifyInsertFailure(r)
          expect(r.body['api:error']['@type']).to.equal('api:KeyFieldsIsEmpty')
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
        })
      }
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
      await document
        .insert(agent, docPath, { schema })
        .then(document.verifyInsertSuccess)

      const instance = {
        '@type': Container,
        contains: [
          {
            '@type': Choice,
            integer: 12,
          },
        ],
      }
      await document
        .insert(agent, docPath, { instance })
        .then(document.verifyInsertSuccess)
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
      await document.insert(agent, docPath, { schema }).then(document.verifyInsertSuccess)
      const r = await document
        .get(agent, docPath, { query: { graph_type: 'schema', id: schema['@id'] } })
        .then(document.verifyGetSuccess)
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
      await document.insert(agent, docPath, { schema }).then(document.verifyInsertSuccess)
      const r = await document
        .get(agent, docPath, { query: { graph_type: 'schema', id: schema['@id'], as_list: true } })
        .then(document.verifyGetSuccess)
      expect(r.body).to.deep.include.members(schema)
    })

    it('fails insert instance with unknown property (#1030)', async function () {
      const schema = { '@id': util.randomString(), '@type': 'Class' }
      await document.insert(agent, docPath, { schema }).then(document.verifyInsertSuccess)
      const instance = { '@type': schema['@id'], unknownProperty: 'abc' }
      const r = await document.insert(agent, docPath, { instance }).then(document.verifyInsertFailure)
      expect(r.body['api:error']['@type']).to.equal('api:UnrecognizedProperty')
      expect(r.body['api:error']['api:document']).to.deep.equal(instance)
      expect(r.body['api:error']['api:property']).to.equal('unknownProperty')
      expect(r.body['api:error']['api:type']).to.equal(schema['@id'])
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
          .insert(agent, docPath, {
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
        await document
          .insert(agent, docPath, {
            instance: { '@type': card, a: [42] },
          })
          .then(document.verifyInsertSuccess)
      })

      it('responds with failure for card', async function () {
        await document
          .insert(agent, docPath, {
            instance: { '@type': card },
          })
          .then(document.verifyInsertFailure)
      })

      it('responds with success for min', async function () {
        await document
          .insert(agent, docPath, {
            instance: { '@type': min, a: [42, 43] },
          })
          .then(document.verifyInsertSuccess)
      })

      it('responds with failure for min', async function () {
        await document
          .insert(agent, docPath, {
            instance: { '@type': min },
          })
          .then(document.verifyInsertFailure)
      })

      it('responds with success for max', async function () {
        await document
          .insert(agent, docPath, {
            instance: { '@type': max, a: [42, 43] },
          })
          .then(document.verifyInsertSuccess)
      })

      it('responds with success for nothing in max', async function () {
        await document
          .insert(agent, docPath, {
            instance: { '@type': max },
          })
          .then(document.verifyInsertSuccess)
      })

      it('responds with failure for max', async function () {
        await document
          .insert(agent, docPath, {
            instance: { '@type': max, a: [42, 23, 12] },
            message: 'message',
          })
          .then(document.verifyInsertFailure)
      })

      it('responds with success for minmax', async function () {
        await document
          .insert(agent, docPath, {
            instance: { '@type': minmax, a: [42, 43] },
          })
          .then(document.verifyInsertSuccess)
      })

      it('responds with failure for under minmax', async function () {
        await document
          .insert(agent, docPath, {
            instance: { '@type': minmax },
          })
          .then(document.verifyInsertFailure)
      })

      it('responds with failure for over minmax', async function () {
        await document
          .insert(agent, docPath, {
            instance: { '@type': minmax, a: [42, 23, 12] },
          })
          .then(document.verifyInsertFailure)
      })
    })
  })
})
