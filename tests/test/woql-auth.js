const { expect } = require('chai')
const { Agent, db, document, endpoint, util, woql } = require('../lib')

const randomType0 = util.randomString()
const randomType1 = util.randomString()

describe('woql-auth', function () {
  let agent
  let dbDefaults
  let dbPath
  let docPath
  let woqlPath

  before(async function () {
    agent = new Agent().auth()
    dbDefaults = endpoint.db(agent.defaults())
    dbPath = dbDefaults.path
    docPath = endpoint.document(dbDefaults).path
    woqlPath = endpoint.woqlResource(dbDefaults).path
    await db.createAfterDel(agent, dbPath)
  })

  after(async function () {
    await db.del(agent, dbPath)
  })

  const simpleQuery = {
    query: {
      '@type': 'Equals',
      left: { '@type': 'True' },
      right: { '@type': 'True' },
    },
  }

  it('accepts simple query', async function () {
    const r = await woql
      .post(agent, woqlPath, simpleQuery)
      .then(woql.verifyGetSuccess)
    expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    expect(r.body.bindings[0]).to.deep.equal({})
    expect(r.body.deletes).to.equal(0)
    expect(r.body.inserts).to.equal(0)
    expect(r.body.transaction_retry_count).to.equal(0)
  })

  const anInsertDocumentQuery = {
    query: {
      '@type': 'InsertDocument',
      identifier: { '@type': 'NodeValue', node: randomType0 + '/0' },
      document: {
        '@type': 'Value',
        dictionary: {
          '@type': 'DictionaryTemplate',
          data: [
            {
              '@type': 'FieldValuePair',
              field: '@type',
              value: { '@type': 'Value', data: randomType0 },
            },
            {
              '@type': 'FieldValuePair',
              field: '@id',
              value: { '@type': 'Value', data: randomType0 + '/0' },
            },
            {
              '@type': 'FieldValuePair',
              field: 'something',
              value: { '@type': 'Value', data: 'a default something' },
            },
          ],
        },
      },
    },
  }

  it('fails InsertDocument without schema', async function () {
    const r = await woql
      .post(agent, woqlPath, anInsertDocumentQuery)
      .then(woql.verifyGetFailure)
    expect(r.body['api:error']['@type']).to.equal('api:TypeNotFound')
    expect(r.body['api:error']['api:type']).to.equal(randomType0)
  })

  describe('with schema', function () {
    const schema = {
      '@id': randomType0,
      '@type': 'Class',
      something: 'xsd:string',
    }

    before(async function () {
      await document
        .insert(agent, docPath, { schema: schema })
        .then(document.verifyInsertSuccess)
    })

    it('fails InsertDocument with bad cast', async function () {
      const query = {
        commit_info: { author: 'a', message: 'm' },
        query: {
          '@type': 'InsertDocument',
          identifier: { '@type': 'NodeValue', node: randomType0 + '/0' },
          document: {
            '@type': 'Value',
            dictionary: {
              '@type': 'DictionaryTemplate',
              data: [
                {
                  '@type': 'FieldValuePair',
                  field: '@type',
                  value: { '@type': 'Value', data: randomType0 },
                },
                {
                  '@type': 'FieldValuePair',
                  field: '@id',
                  value: { '@type': 'Value', data: randomType0 + '/0' },
                },
                {
                  '@type': 'FieldValuePair',
                  field: 'something',
                  value: { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 'STRING' } },
                },
              ],
            },
          },
        },
      }
      const r = await woql.post(agent, woqlPath, query).then(woql.verifyGetFailure)
      expect(r.body['api:error']['@type']).to.equal('api:BadCast')
      expect(r.body['api:error']['api:type']).to.equal('http://www.w3.org/2001/XMLSchema#integer')
      expect(r.body['api:error']['api:value']).to.equal('STRING')
    })

    it('fails InsertDocument without commit_info', async function () {
      const r = await woql
        .post(agent, woqlPath, anInsertDocumentQuery)
        .then(woql.verifyGetFailure)
      expect(r.body['api:error']['@type']).to.equal('api:MissingParameter')
      expect(r.body['api:error']['api:parameter']).to.equal('author')
    })

    it('fails InsertDocument without commit_info.message', async function () {
      const query = { commit_info: { author: 'a' } }
      Object.assign(query, anInsertDocumentQuery)
      const r = await woql
        .post(agent, woqlPath, query)
        .then(woql.verifyGetFailure)
      expect(r.body['api:error']['@type']).to.equal('api:MissingParameter')
      expect(r.body['api:error']['api:parameter']).to.equal('message')
    })

    it('passes InsertDocument, ReadDocument, UpdateDocument, DeleteDocument', async function () {
      const query = { commit_info: { author: 'a', message: 'm' } }
      Object.assign(query, anInsertDocumentQuery)

      {
        // InsertDocument
        const r = await woql
          .post(agent, woqlPath, query)
          .then(woql.verifyGetSuccess)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        expect(r.body.bindings[0]).to.deep.equal({})
        expect(r.body.deletes).to.equal(0)
        expect(r.body.inserts).to.equal(2)
        expect(r.body.transaction_retry_count).to.equal(0)
      }

      // Change query to UpdateDocument
      query.query['@type'] = 'UpdateDocument'
      const aNewSomething = 'a new something'
      query.query.document.dictionary.data[2].value.data = aNewSomething

      {
        // UpdateDocument
        const r = await woql
          .post(agent, woqlPath, query)
          .then(woql.verifyGetSuccess)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        expect(r.body.bindings[0]).to.deep.equal({})
        expect(r.body.deletes).to.equal(1)
        expect(r.body.inserts).to.equal(1)
        expect(r.body.transaction_retry_count).to.equal(0)
      }

      // Change query to ReadDocument
      query.query['@type'] = 'ReadDocument'
      query.query.document = { '@type': 'DataValue', variable: 'X' }

      {
        // ReadDocument
        const r = await woql
          .post(agent, woqlPath, query)
          .then(woql.verifyGetSuccess)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(1)
        expect(r.body['api:variable_names'][0]).to.equal('X')
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        expect(r.body.bindings[0]).to.deep.equal({
          X: { '@type': randomType0, '@id': randomType0 + '/0', something: aNewSomething },
        })
        expect(r.body.deletes).to.equal(0)
        expect(r.body.inserts).to.equal(0)
        expect(r.body.transaction_retry_count).to.equal(0)
      }

      // Change query to DeleteDocument
      query.query['@type'] = 'DeleteDocument'
      delete query.query.document

      {
        // DeleteDocument
        const r = await woql
          .post(agent, woqlPath, query)
          .then(woql.verifyGetSuccess)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        expect(r.body.bindings[0]).to.deep.equal({})
        expect(r.body.deletes).to.equal(2)
        expect(r.body.inserts).to.equal(0)
        expect(r.body.transaction_retry_count).to.equal(0)
      }
    })
  })

  describe('key type: Random', function () {
    const schema = {
      '@id': randomType1,
      '@key': { '@type': 'Random' },
      '@type': 'Class',
      label: 'xsd:string',
    }
    const instance = {
      '@type': randomType1,
      label: 'Neel was here first.',
    }
    let id
    let idFull

    before(async function () {
      await document.insert(agent, docPath, { schema }).then(document.verifyInsertSuccess)
      const r = await document.insert(agent, docPath, { instance }).then(document.verifyInsertSuccess)
      expect(r.body).to.be.an('array').that.has.lengthOf(1)
      idFull = r.body[0]
      id = idFull.replace('terminusdb:///data/', '')
    })

    after(async function () {
      await document.del(agent, docPath, { query: { id } }).then(document.verifyDelSuccess)
    })

    function queryTemplate () {
      return {
        commit_info: { author: 'Not Neel', message: 'This is somebody else.' },
        query: {
          '@type': 'UpdateDocument',
          document: {
            '@type': 'Value',
            dictionary: {
              '@type': 'DictionaryTemplate',
              data: [
                {
                  '@type': 'FieldValuePair',
                  field: 'label',
                  value: {
                    '@type': 'Value',
                    data: { '@type': 'xsd:string' },
                  },
                },
                {
                  '@type': 'FieldValuePair',
                  field: '@type',
                  value: {
                    '@type': 'Value',
                    data: { '@type': 'xsd:string', '@value': randomType1 },
                  },
                },
              ],
            },
          },
        },
      }
    }

    it('fails UpdateDocument with node identifier and missing @id', async function () {
      const query = queryTemplate()
      query.query.identifier = { '@type': 'NodeValue', node: id }
      query.query.document.dictionary.data[0].value.data['@value'] = util.randomString()
      const r = await woql.post(agent, woqlPath, query).then(woql.verifyGetFailure)
      expect(r.body['api:error']).to.have.property('@type').that.equals('api:SubmittedIdDoesNotMatchGeneratedId')
      expect(r.body['api:error']).to.have.property('api:submitted_id').that.equals(idFull)
      const re = new RegExp('^' + 'terminusdb:///data/' + randomType1 + '/')
      expect(r.body['api:error']).to.have.property('api:generated_id').that.matches(re)
    })

    it('fails UpdateDocument with missing @id', async function () {
      const query = queryTemplate()
      query.query.document.dictionary.data[0].value.data['@value'] = util.randomString()
      const r = await woql.post(agent, woqlPath, query).then(woql.verifyNotFound)
      expect(r.body['api:error']['@type']).to.equal('api:DocumentNotFound')
      expect(r.body['api:error']['api:document']).to.deep.equal({
        '@type': randomType1,
        label: query.query.document.dictionary.data[0].value.data['@value'],
      })
      const re = new RegExp('^' + 'terminusdb:///data/' + randomType1 + '/')
      expect(r.body['api:error']).to.have.property('api:document_id').that.matches(re)
    })

    it('passes UpdateDocument with node identifier', async function () {
      const query = queryTemplate()
      query.query.identifier = { '@type': 'NodeValue', node: id }
      query.query.document.dictionary.data.push({
        '@type': 'FieldValuePair',
        field: '@id',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': id },
        },
      })
      query.query.document.dictionary.data[0].value.data['@value'] = util.randomString()
      {
        // UpdateDocument
        const r = await woql.post(agent, woqlPath, query).then(woql.verifyGetSuccess)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        expect(r.body.bindings[0]).to.deep.equal({})
        expect(r.body.deletes).to.equal(1)
        expect(r.body.inserts).to.equal(1)
        expect(r.body.transaction_retry_count).to.equal(0)
      }
      {
        // GET
        const r = await document.get(agent, docPath, { body: { id } }).then(document.verifyGetSuccess)
        expect(r.body['@id']).to.equal(id)
        expect(r.body['@type']).to.equal(randomType1)
        expect(r.body.label).to.equal(query.query.document.dictionary.data[0].value.data['@value'])
      }
    })

    it('passes UpdateDocument with variable identifier', async function () {
      const query = queryTemplate()
      query.query.identifier = { '@type': 'NodeValue', variable: 'Id' }
      query.query.document.dictionary.data.push({
        '@type': 'FieldValuePair',
        field: '@id',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': idFull },
        },
      })
      query.query.document.dictionary.data[0].value.data['@value'] = util.randomString()
      {
        // UpdateDocument
        const r = await woql.post(agent, woqlPath, query).then(woql.verifyGetSuccess)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(1)
        expect(r.body['api:variable_names'][0]).to.equal(query.query.identifier.variable)
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        const object = {}
        object[query.query.identifier.variable] = id
        expect(r.body.bindings[0]).to.deep.equal(object)
        expect(r.body.deletes).to.equal(1)
        expect(r.body.inserts).to.equal(1)
        expect(r.body.transaction_retry_count).to.equal(0)
      }
      {
        // GET
        const r = await document.get(agent, docPath, { body: { id } }).then(document.verifyGetSuccess)
        expect(r.body['@id']).to.equal(id)
        expect(r.body['@type']).to.equal(randomType1)
        expect(r.body.label).to.equal(query.query.document.dictionary.data[0].value.data['@value'])
      }
    })
  })
})
