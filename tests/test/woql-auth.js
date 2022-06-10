const { expect } = require('chai')
const { Agent, api, db, document, util, woql } = require('../lib')

const randomType0 = util.randomString()
const randomType1 = util.randomString()

describe('woql-auth', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  const simpleQuery = {
    '@type': 'Equals',
    left: { '@type': 'True' },
    right: { '@type': 'True' },
  }

  it('accepts simple query', async function () {
    const r = await woql.post(agent, simpleQuery)
    expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    expect(r.body.bindings[0]).to.deep.equal({})
    expect(r.body.deletes).to.equal(0)
    expect(r.body.inserts).to.equal(0)
    expect(r.body.transaction_retry_count).to.equal(0)
  })

  function anInsertDocumentQuery () {
    return {
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
    }
  }

  it('fails InsertDocument without schema', async function () {
    const witness = {
      '@type': 'ascribed_type_does_not_exist',
      ascribed_type: `terminusdb:///schema#${randomType0}`,
      document: {
        '@id': `${randomType0}/0`,
        '@type': randomType0,
        something: 'a default something',
      },
    }
    await woql
      .post(agent, anInsertDocumentQuery())
      .fails(api.error.woqlSchemaCheckFailure([witness]))
  })

  describe('with schema', function () {
    const schema = {
      '@id': randomType0,
      '@type': 'Class',
      something: 'xsd:string',
    }

    before(async function () {
      await document.insert(agent, { schema })
    })

    it('fails InsertDocument with bad cast', async function () {
      const query = {
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
      }
      await woql
        .post(agent, query)
        .fails(api.error.badCast('http://www.w3.org/2001/XMLSchema#integer', 'STRING'))
    })

    it('fails InsertDocument without commit_info.author', async function () {
      await woql
        .post(agent, anInsertDocumentQuery(), { commitInfo: {} })
        .fails(api.error.missingParameter('author'))
    })

    it('fails InsertDocument without commit_info.message', async function () {
      await woql
        .post(agent, anInsertDocumentQuery(), { commitInfo: { author: 'default_author' } })
        .fails(api.error.missingParameter('message'))
    })

    it('passes InsertDocument, ReadDocument, UpdateDocument, DeleteDocument', async function () {
      const query = anInsertDocumentQuery()

      {
        // InsertDocument
        const r = await woql.post(agent, query)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        expect(r.body.bindings[0]).to.deep.equal({})
        expect(r.body.deletes).to.equal(0)
        expect(r.body.inserts).to.equal(2)
        expect(r.body.transaction_retry_count).to.equal(0)
      }

      // Change query to UpdateDocument
      query['@type'] = 'UpdateDocument'
      const aNewSomething = 'a new something'
      query.document.dictionary.data[2].value.data = aNewSomething

      {
        // UpdateDocument
        const r = await woql.post(agent, query)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        expect(r.body.bindings[0]).to.deep.equal({})
        expect(r.body.deletes).to.equal(1)
        expect(r.body.inserts).to.equal(1)
        expect(r.body.transaction_retry_count).to.equal(0)
      }

      // Change query to ReadDocument
      query['@type'] = 'ReadDocument'
      query.document = { '@type': 'DataValue', variable: 'X' }

      {
        // ReadDocument
        const r = await woql.post(agent, query)
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
      query['@type'] = 'DeleteDocument'
      delete query.document

      {
        // DeleteDocument
        const r = await woql.post(agent, query)
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
      await document.insert(agent, { schema })
      const r = await document.insert(agent, { instance })
      expect(r.body).to.be.an('array').that.has.lengthOf(1)
      idFull = r.body[0]
      id = idFull.replace('terminusdb:///data/', '')
    })

    after(async function () {
      await document.delete(agent, { query: { id } })
    })

    function queryTemplate () {
      return {
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
      }
    }

    it('fails UpdateDocument with node identifier and missing @id', async function () {
      const query = queryTemplate()
      query.identifier = { '@type': 'NodeValue', node: id }
      query.document.dictionary.data[0].value.data['@value'] = util.randomString()
      const r = await woql.post(agent, query).fails()
      expect(r.body['api:error']).to.have.property('@type').that.equals('api:SubmittedIdDoesNotMatchGeneratedId')
      expect(r.body['api:error']).to.have.property('api:submitted_id').that.equals(idFull)
      const re = new RegExp('^' + 'terminusdb:///data/' + randomType1 + '/')
      expect(r.body['api:error']).to.have.property('api:generated_id').that.matches(re)
    })

    it('fails UpdateDocument with missing @id', async function () {
      const query = queryTemplate()
      query.document.dictionary.data[0].value.data['@value'] = util.randomString()
      const r = await woql.post(agent, query).notFound()
      expect(r.body['api:error']['@type']).to.equal('api:DocumentNotFound')
      expect(r.body['api:error']['api:document']).to.deep.equal({
        '@type': randomType1,
        label: query.document.dictionary.data[0].value.data['@value'],
      })
      const re = new RegExp('^' + 'terminusdb:///data/' + randomType1 + '/')
      expect(r.body['api:error']).to.have.property('api:document_id').that.matches(re)
    })

    it('passes UpdateDocument with node identifier', async function () {
      const query = queryTemplate()
      query.identifier = { '@type': 'NodeValue', node: id }
      query.document.dictionary.data.push({
        '@type': 'FieldValuePair',
        field: '@id',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': id },
        },
      })
      query.document.dictionary.data[0].value.data['@value'] = util.randomString()
      {
        // UpdateDocument
        const r = await woql.post(agent, query)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        expect(r.body.bindings[0]).to.deep.equal({})
        expect(r.body.deletes).to.equal(1)
        expect(r.body.inserts).to.equal(1)
        expect(r.body.transaction_retry_count).to.equal(0)
      }
      {
        // GET
        const r = await document.get(agent, { body: { id } })
        expect(r.body['@id']).to.equal(id)
        expect(r.body['@type']).to.equal(randomType1)
        expect(r.body.label).to.equal(query.document.dictionary.data[0].value.data['@value'])
      }
    })

    it('passes UpdateDocument with variable identifier', async function () {
      const query = queryTemplate()
      query.identifier = { '@type': 'NodeValue', variable: 'Id' }
      query.document.dictionary.data.push({
        '@type': 'FieldValuePair',
        field: '@id',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': idFull },
        },
      })
      query.document.dictionary.data[0].value.data['@value'] = util.randomString()
      {
        // UpdateDocument
        const r = await woql.post(agent, query)
        expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(1)
        expect(r.body['api:variable_names'][0]).to.equal(query.identifier.variable)
        expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
        const object = {}
        object[query.identifier.variable] = id
        expect(r.body.bindings[0]).to.deep.equal(object)
        expect(r.body.deletes).to.equal(1)
        expect(r.body.inserts).to.equal(1)
        expect(r.body.transaction_retry_count).to.equal(0)
      }
      {
        // GET
        const r = await document.get(agent, { body: { id } })
        expect(r.body['@id']).to.equal(id)
        expect(r.body['@type']).to.equal(randomType1)
        expect(r.body.label).to.equal(query.document.dictionary.data[0].value.data['@value'])
      }
    })
  })
})
