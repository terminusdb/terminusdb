const { expect } = require('chai')
const { Agent, api, db, document } = require('../lib')

describe('schema-check-coverage', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  describe('Abstract class instantiation checks', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates abstract class successfully', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'Animal',
        '@abstract': [],
        name: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('creates concrete subclass of abstract class', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'Dog',
        '@inherits': ['Animal'],
        breed: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('inserts instance of concrete subclass successfully', async function () {
      const instance = {
        '@type': 'Dog',
        name: 'Buddy',
        breed: 'Golden Retriever',
      }
      await document.insert(agent, { instance })
    })

    it('rejects instance of abstract class directly', async function () {
      const instance = {
        '@type': 'Animal',
        '@id': 'Animal/invalid',
        name: 'Generic Animal',
      }
      const witness = {
        '@type': 'reifying_abstract_class',
        class: 'terminusdb:///schema#Animal',
        subject: 'terminusdb:///data/Animal/invalid',
      }
      await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
    })

    it('creates multiple levels of abstract inheritance', async function () {
      const schemas = [
        {
          '@type': 'Class',
          '@id': 'Mammal',
          '@abstract': [],
          '@inherits': ['Animal'],
          warmBlooded: 'xsd:boolean',
        },
        {
          '@type': 'Class',
          '@id': 'Cat',
          '@inherits': ['Mammal'],
          indoor: 'xsd:boolean',
        },
      ]
      await document.insert(agent, { schema: schemas })
    })

    it('inserts instance of deeply inherited concrete class', async function () {
      const instance = {
        '@type': 'Cat',
        name: 'Whiskers',
        warmBlooded: true,
        indoor: true,
      }
      await document.insert(agent, { instance })
    })

    it('rejects instance of intermediate abstract class', async function () {
      const instance = {
        '@type': 'Mammal',
        '@id': 'Mammal/invalid',
        name: 'Generic Mammal',
        warmBlooded: true,
      }
      const witness = {
        '@type': 'reifying_abstract_class',
        class: 'terminusdb:///schema#Mammal',
        subject: 'terminusdb:///data/Mammal/invalid',
      }
      await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
    })
  })

  describe('Subdocument constraint checks', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('fails to create subdocument without @key', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'InvalidSubdoc',
        '@subdocument': [],
        value: 'xsd:string',
      }
      await document.insert(agent, { schema }).fails(api.error.subdocumentKeyMissing(schema))
    })

    it('creates subdocument with valid @key', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'Address',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        street: 'xsd:string',
        city: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('creates parent class with subdocument field', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'Person',
        name: 'xsd:string',
        address: { '@type': 'Optional', '@class': 'Address' },
      }
      await document.insert(agent, { schema })
    })

    it('inserts document with embedded subdocument', async function () {
      const instance = {
        '@type': 'Person',
        name: 'John Doe',
        address: {
          '@type': 'Address',
          street: '123 Main St',
          city: 'Springfield',
        },
      }
      await document.insert(agent, { instance })
    })

    it('creates subdocument with Lexical key', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'PhoneNumber',
        '@subdocument': [],
        '@key': { '@type': 'Lexical', '@fields': ['number'] },
        number: 'xsd:string',
        type: { '@type': 'Optional', '@class': 'xsd:string' },
      }
      await document.insert(agent, { schema })
    })

    it('creates subdocument with ValueHash key', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'Metadata',
        '@subdocument': [],
        '@key': { '@type': 'ValueHash' },
        key: 'xsd:string',
        value: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('fails subdocument with invalid @key type', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'BadKeySubdoc',
        '@subdocument': [],
        '@key': { '@type': 'InvalidKeyType' },
        data: 'xsd:string',
      }
      await document.insert(agent, { schema }).fails(api.error.documentKeyTypeUnknown(schema))
    })

    it('creates nested subdocuments', async function () {
      const schemas = [
        {
          '@type': 'Class',
          '@id': 'InnerSubdoc',
          '@subdocument': [],
          '@key': { '@type': 'Random' },
          innerValue: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'OuterSubdoc',
          '@subdocument': [],
          '@key': { '@type': 'Random' },
          inner: 'InnerSubdoc',
        },
        {
          '@type': 'Class',
          '@id': 'Container',
          outer: 'OuterSubdoc',
        },
      ]
      await document.insert(agent, { schema: schemas })
    })

    it('inserts document with nested subdocuments', async function () {
      const instance = {
        '@type': 'Container',
        outer: {
          '@type': 'OuterSubdoc',
          inner: {
            '@type': 'InnerSubdoc',
            innerValue: 'deeply nested',
          },
        },
      }
      await document.insert(agent, { instance })
    })
  })

  describe('Documentation validation checks', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates class with simple @documentation', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'DocumentedClass',
        '@documentation': {
          '@comment': 'This is a documented class',
        },
        field: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('creates class with @documentation including @properties', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'FullyDocumented',
        '@documentation': {
          '@comment': 'A fully documented class',
          '@properties': {
            name: 'The name of the entity',
            count: 'The number of items',
          },
        },
        name: 'xsd:string',
        count: 'xsd:integer',
      }
      await document.insert(agent, { schema })
    })

    it('creates class with multi-language @documentation', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'MultiLangDoc',
        '@documentation': [
          {
            '@language': 'en',
            '@label': 'Multi-Language Document',
            '@comment': 'This class has documentation in multiple languages',
          },
          {
            '@language': 'es',
            '@label': 'Documento Multi-Idioma',
            '@comment': 'Esta clase tiene documentación en múltiples idiomas',
          },
        ],
        content: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('creates class with @documentation without @comment', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'PropertiesOnlyDoc',
        '@documentation': {
          '@properties': {
            value: 'The value field',
          },
        },
        value: 'xsd:integer',
      }
      await document.insert(agent, { schema })
    })

    it('creates enum with @documentation including @values', async function () {
      const schema = {
        '@type': 'Enum',
        '@id': 'Status',
        '@documentation': {
          '@comment': 'Status enumeration',
          '@values': {
            active: 'The item is active',
            inactive: 'The item is inactive',
            pending: 'The item is pending review',
          },
        },
        '@value': ['active', 'inactive', 'pending'],
      }
      await document.insert(agent, { schema })
    })

    it('creates subdocument with @documentation', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'DocumentedSubdoc',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        '@documentation': {
          '@comment': 'A documented subdocument',
          '@properties': {
            data: 'The data field of the subdocument',
          },
        },
        data: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('retrieves schema with @documentation intact', async function () {
      const result = await document.get(agent, { query: { id: 'DocumentedClass', graph_type: 'schema' } }).unverified()
      expect(result.status).to.equal(200)
      const schema = JSON.parse(result.text)
      expect(schema['@documentation']).to.deep.equal({
        '@comment': 'This is a documented class',
      })
    })

    it('retrieves enum schema with @values documentation', async function () {
      const result = await document.get(agent, { query: { id: 'Status', graph_type: 'schema' } }).unverified()
      expect(result.status).to.equal(200)
      const schema = JSON.parse(result.text)
      expect(schema['@documentation']['@values']).to.deep.equal({
        active: 'The item is active',
        inactive: 'The item is inactive',
        pending: 'The item is pending review',
      })
    })
  })

  describe('Schema change after instance insertion', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates initial schema and instance', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'InitialClass',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema })

      const instance = {
        '@type': 'InitialClass',
        name: 'Test Instance',
      }
      await document.insert(agent, { instance })
    })

    it('adds new unrelated class after instance exists', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'UnrelatedClass',
        value: 'xsd:integer',
      }
      await document.insert(agent, { schema })
    })

    it('adds new enum after instance exists', async function () {
      const schema = {
        '@type': 'Enum',
        '@id': 'Priority',
        '@value': ['low', 'medium', 'high'],
      }
      await document.insert(agent, { schema })
    })

    it('adds abstract class after instance exists', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'AbstractAfterInstance',
        '@abstract': [],
        description: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('adds subdocument class after instance exists', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'SubdocAfterInstance',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        data: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('retrieves original instance after all schema additions', async function () {
      const result = await document.get(agent, { query: { type: 'InitialClass' } }).unverified()
      expect(result.status).to.equal(200)
      const docs = JSON.parse('[' + result.text.trim().split('\n').join(',') + ']')
      expect(docs).to.have.lengthOf(1)
      expect(docs[0].name).to.equal('Test Instance')
    })
  })

  describe('Key constraint validation', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates class with Lexical key', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'LexicalKeyed',
        '@key': { '@type': 'Lexical', '@fields': ['code'] },
        code: 'xsd:string',
        description: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('creates class with Hash key', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'HashKeyed',
        '@key': { '@type': 'Hash', '@fields': ['value'] },
        value: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('creates class with ValueHash key', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'ValueHashKeyed',
        '@key': { '@type': 'ValueHash' },
        content: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('creates class with Random key', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'RandomKeyed',
        '@key': { '@type': 'Random' },
        data: 'xsd:string',
      }
      await document.insert(agent, { schema })
    })

    it('fails with unknown key type', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'UnknownKeyType',
        '@key': { '@type': 'Unknown' },
        field: 'xsd:string',
      }
      await document.insert(agent, { schema }).fails(api.error.documentKeyTypeUnknown(schema))
    })

    it('fails with Lexical key missing @fields', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'LexicalMissingFields',
        '@key': { '@type': 'Lexical' },
        field: 'xsd:string',
      }
      await document.insert(agent, { schema }).fails(api.error.keyMissingFields(schema))
    })

    it('fails with Lexical key with empty @fields', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'LexicalEmptyFields',
        '@key': { '@type': 'Lexical', '@fields': [] },
        field: 'xsd:string',
      }
      await document.insert(agent, { schema }).fails(api.error.keyFieldsIsEmpty(schema))
    })

    it('fails with @key not an object', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'KeyNotObject',
        '@key': 'invalid',
        field: 'xsd:string',
      }
      await document.insert(agent, { schema }).fails(api.error.documentKeyNotObject(schema))
    })
  })
})
