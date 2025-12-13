const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('foreign-field-multiplicity', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  describe('Foreign field with standard Optional syntax', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates schema with Foreign type', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })
    })

    it('creates class with Optional Foreign field using standard syntax', async function () {
      const classSchema = {
        '@type': 'Class',
        '@id': 'Employee',
        person: {
          '@class': 'Person',
          '@type': 'Optional',
        },
      }
      await document.insert(agent, { schema: classSchema })
    })

    it('inserts document with Optional Foreign field', async function () {
      const instance = {
        '@type': 'Employee',
        person: 'http://example.com/person/1',
      }
      await document.insert(agent, { instance })
    })

    it('allows adding new schema class after instance with Foreign field exists', async function () {
      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })
    })
  })

  describe('Foreign field with standard Mandatory syntax', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates schema with Foreign type', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })
    })

    it('creates class with Mandatory Foreign field using standard syntax', async function () {
      const classSchema = {
        '@type': 'Class',
        '@id': 'Employee',
        person: 'Person',
      }
      await document.insert(agent, { schema: classSchema })
    })

    it('inserts document with Mandatory Foreign field', async function () {
      const instance = {
        '@type': 'Employee',
        person: 'http://example.com/person/1',
      }
      await document.insert(agent, { instance })
    })

    it('allows adding new schema class after instance with Foreign field exists', async function () {
      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })
    })
  })

  describe('Foreign field with standard Set syntax', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates schema with Foreign type', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })
    })

    it('creates class with Set Foreign field using standard syntax', async function () {
      const classSchema = {
        '@type': 'Class',
        '@id': 'Team',
        members: {
          '@class': 'Person',
          '@type': 'Set',
        },
      }
      await document.insert(agent, { schema: classSchema })
    })

    it('inserts document with Set Foreign field', async function () {
      const instance = {
        '@type': 'Team',
        members: ['http://example.com/person/1', 'http://example.com/person/2'],
      }
      await document.insert(agent, { instance })
    })

    it('allows adding new schema class after instance with Foreign field exists', async function () {
      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })
    })
  })

  describe('Foreign field with standard Cardinality syntax', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates schema with Foreign type', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })
    })

    it('creates class with Cardinality Foreign field using standard syntax', async function () {
      const classSchema = {
        '@type': 'Class',
        '@id': 'Project',
        leads: {
          '@class': 'Person',
          '@type': 'Cardinality',
          '@min_cardinality': 1,
          '@max_cardinality': 3,
        },
      }
      await document.insert(agent, { schema: classSchema })
    })

    it('inserts document with Cardinality Foreign field', async function () {
      const instance = {
        '@type': 'Project',
        leads: ['http://example.com/person/1', 'http://example.com/person/2'],
      }
      await document.insert(agent, { instance })
    })

    it('allows adding new schema class after instance with Foreign field exists', async function () {
      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })
    })
  })

  describe('Foreign field with standard Array syntax', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates schema with Foreign type', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })
    })

    it('creates class with Array Foreign field using standard syntax', async function () {
      const classSchema = {
        '@type': 'Class',
        '@id': 'Queue',
        people: {
          '@class': 'Person',
          '@type': 'Array',
        },
      }
      await document.insert(agent, { schema: classSchema })
    })

    it('inserts document with Array Foreign field', async function () {
      const instance = {
        '@type': 'Queue',
        people: ['http://example.com/person/1', 'http://example.com/person/2'],
      }
      await document.insert(agent, { instance })
    })

    it('allows adding new schema class after instance with Foreign field exists', async function () {
      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })
    })
  })

  describe('Foreign field with standard List syntax', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates schema with Foreign type', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })
    })

    it('creates class with List Foreign field using standard syntax', async function () {
      const classSchema = {
        '@type': 'Class',
        '@id': 'Roster',
        lineup: {
          '@class': 'Person',
          '@type': 'List',
        },
      }
      await document.insert(agent, { schema: classSchema })
    })

    it('inserts document with List Foreign field', async function () {
      const instance = {
        '@type': 'Roster',
        lineup: ['http://example.com/person/1', 'http://example.com/person/2'],
      }
      await document.insert(agent, { instance })
    })

    it('allows adding new schema class after instance with Foreign field exists', async function () {
      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })
    })
  })

  describe('Foreign field without instance - schema changes should work', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates Foreign and class with Optional field, then adds new class without instances', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })

      const classSchema = {
        '@type': 'Class',
        '@id': 'Employee',
        person: {
          '@class': 'Person',
          '@type': 'Optional',
        },
      }
      await document.insert(agent, { schema: classSchema })

      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })
    })
  })

  describe('Foreign field - document without Foreign value', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates and uses Optional Foreign field without setting value', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })

      const classSchema = {
        '@type': 'Class',
        '@id': 'Employee',
        name: 'xsd:string',
        person: {
          '@class': 'Person',
          '@type': 'Optional',
        },
      }
      await document.insert(agent, { schema: classSchema })

      const instance = {
        '@type': 'Employee',
        name: 'John Doe',
      }
      await document.insert(agent, { instance })

      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })
    })
  })

  describe('Foreign field - multiple documents then schema change', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates multiple documents with Foreign fields, then adds new schema', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })

      const classSchema = {
        '@type': 'Class',
        '@id': 'Employee',
        person: {
          '@class': 'Person',
          '@type': 'Optional',
        },
      }
      await document.insert(agent, { schema: classSchema })

      const instances = [
        { '@type': 'Employee', person: 'http://example.com/person/1' },
        { '@type': 'Employee', person: 'http://example.com/person/2' },
        { '@type': 'Employee' },
      ]
      await document.insert(agent, { instance: instances })

      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })

      const anotherInstance = {
        '@type': 'Department',
        name: 'Engineering',
      }
      await document.insert(agent, { instance: anotherInstance })
    })
  })

  describe('Foreign field - retrieve document after schema change', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('retrieves documents correctly after adding new schema class', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })

      const classSchema = {
        '@type': 'Class',
        '@id': 'Employee',
        '@key': { '@type': 'Lexical', '@fields': ['name'] },
        name: 'xsd:string',
        person: {
          '@class': 'Person',
          '@type': 'Optional',
        },
      }
      await document.insert(agent, { schema: classSchema })

      const instance = {
        '@type': 'Employee',
        name: 'test',
        person: 'http://example.com/person/1',
      }
      await document.insert(agent, { instance })

      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })

      const result = await document.get(agent, { query: { type: 'Employee' } }).unverified()
      expect(result.status).to.equal(200)
      const docs = JSON.parse('[' + result.text.trim().split('\n').join(',') + ']')
      expect(docs).to.have.lengthOf(1)
      expect(docs[0].person).to.equal('http://example.com/person/1')
    })
  })

  describe('Backward compatibility for Foreign field: Original odd syntax', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('creates schema with Foreign type', async function () {
      const foreignSchema = {
        '@type': 'Foreign',
        '@id': 'Person',
      }
      await document.insert(agent, { schema: foreignSchema })
    })

    it('creates class with Foreign field using workaround syntax', async function () {
      const classSchema = {
        '@type': 'Class',
        '@id': 'Employee',
        person: {
          '@class': 'Person',
          '@type': 'Foreign',
          '@id': 'Person',
        },
      }
      await document.insert(agent, { schema: classSchema })
    })

    it('inserts document with Foreign field value', async function () {
      const instance = {
        '@type': 'Employee',
        person: 'http://example.com/person/1',
      }
      await document.insert(agent, { instance })
    })

    it('allows adding new schema class after instance with Foreign field exists', async function () {
      const newClass = {
        '@type': 'Class',
        '@id': 'Department',
        name: 'xsd:string',
      }
      await document.insert(agent, { schema: newClass })
    })

    it('retrieves document correctly after schema change', async function () {
      const result = await document.get(agent, { query: { type: 'Employee' } }).unverified()
      expect(result.status).to.equal(200)
      const docs = JSON.parse('[' + result.text.trim().split('\n').join(',') + ']')
      expect(docs).to.have.lengthOf(1)
      expect(docs[0].person).to.equal('http://example.com/person/1')
    })

    it('can insert another document of different type after schema change', async function () {
      const instance = {
        '@type': 'Department',
        name: 'Engineering',
      }
      await document.insert(agent, { instance })
    })
  })
})
