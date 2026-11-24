const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('subdocument-deletion', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  describe('nested subdocuments with empty @base', function () {
    const dbName = 'subdoc_empty_base'

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)

      const schema = [
        {
          '@id': 'TopLevel',
          '@type': 'Class',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          level1: 'Level1',
        },
        {
          '@id': 'Level1',
          '@type': 'Class',
          '@subdocument': [],
          '@base': '',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          level2: 'Level2',
        },
        {
          '@id': 'Level2',
          '@type': 'Class',
          '@subdocument': [],
          '@base': '',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          level3: 'Level3',
        },
        {
          '@id': 'Level3',
          '@type': 'Class',
          '@subdocument': [],
          '@base': '',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('deletes nested subdocuments without leaving stray triples', async function () {
      const docToDelete = {
        '@type': 'TopLevel',
        name: 'delete-me',
        level1: {
          '@type': 'Level1',
          name: 'l1',
          level2: {
            '@type': 'Level2',
            name: 'l2',
            level3: {
              '@type': 'Level3',
              name: 'l3',
            },
          },
        },
      }

      const controlDoc = {
        '@type': 'TopLevel',
        name: 'keep-me',
        level1: {
          '@type': 'Level1',
          name: 'l1',
          level2: {
            '@type': 'Level2',
            name: 'l2',
            level3: {
              '@type': 'Level3',
              name: 'l3',
            },
          },
        },
      }

      await document.insert(agent, { instance: docToDelete })
      await document.insert(agent, { instance: controlDoc })

      const docsBefore = await document.get(agent, { query: { type: 'TopLevel', as_list: true } })
      expect(docsBefore.body).to.have.lengthOf(2)

      await document.delete(agent, { query: { id: 'TopLevel/delete-me' } })

      const docsAfter = await document.get(agent, { query: { type: 'TopLevel', as_list: true } })
      expect(docsAfter.body).to.have.lengthOf(1)
      expect(docsAfter.body[0].name).to.equal('keep-me')

      const tripleQuery = {
        '@type': 'Triple',
        subject: { '@type': 'NodeValue', variable: 'Subject' },
        predicate: { '@type': 'NodeValue', variable: 'Predicate' },
        object: { '@type': 'Value', variable: 'Object' },
      }

      const result = await woql.post(agent, tripleQuery)
      const triples = result.body.bindings

      const strayTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && subject.includes('delete-me')
      })

      expect(strayTriples).to.have.lengthOf(0,
        `Found stray triples for deleted document: ${JSON.stringify(strayTriples, null, 2)}`)

      const controlTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && subject.includes('keep-me')
      })

      expect(controlTriples.length).to.be.greaterThan(0,
        'Control document triples should still exist')
    })
  })

  describe('nested subdocuments with custom @base', function () {
    const dbName = 'subdoc_custom_base'

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)

      const schema = [
        {
          '@id': 'TopLevel',
          '@type': 'Class',
          '@base': 'top/',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          level1: 'Level1',
        },
        {
          '@id': 'Level1',
          '@type': 'Class',
          '@subdocument': [],
          '@base': 'first-level/',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          level2: 'Level2',
        },
        {
          '@id': 'Level2',
          '@type': 'Class',
          '@subdocument': [],
          '@base': 'second-level/',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          level3: 'Level3',
        },
        {
          '@id': 'Level3',
          '@type': 'Class',
          '@subdocument': [],
          '@base': 'third-level/',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('deletes nested subdocuments with custom @base without leaving stray triples', async function () {
      const docToDelete = {
        '@type': 'TopLevel',
        name: 'delete-me',
        level1: {
          '@type': 'Level1',
          name: 'l1',
          level2: {
            '@type': 'Level2',
            name: 'l2',
            level3: {
              '@type': 'Level3',
              name: 'l3',
            },
          },
        },
      }

      const controlDoc = {
        '@type': 'TopLevel',
        name: 'keep-me',
        level1: {
          '@type': 'Level1',
          name: 'l1',
          level2: {
            '@type': 'Level2',
            name: 'l2',
            level3: {
              '@type': 'Level3',
              name: 'l3',
            },
          },
        },
      }

      await document.insert(agent, { instance: docToDelete })
      await document.insert(agent, { instance: controlDoc })

      const docsBefore = await document.get(agent, { query: { type: 'TopLevel', as_list: true } })
      expect(docsBefore.body).to.have.lengthOf(2)

      await document.delete(agent, { query: { id: 'top/delete-me' } })

      const docsAfter = await document.get(agent, { query: { type: 'TopLevel', as_list: true } })
      expect(docsAfter.body).to.have.lengthOf(1)
      expect(docsAfter.body[0].name).to.equal('keep-me')

      const tripleQuery = {
        '@type': 'Triple',
        subject: { '@type': 'NodeValue', variable: 'Subject' },
        predicate: { '@type': 'NodeValue', variable: 'Predicate' },
        object: { '@type': 'Value', variable: 'Object' },
      }

      const result = await woql.post(agent, tripleQuery)
      const triples = result.body.bindings

      const strayTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && subject.includes('delete-me')
      })

      expect(strayTriples).to.have.lengthOf(0,
        `Found stray triples for deleted document: ${JSON.stringify(strayTriples, null, 2)}`)

      const controlTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && subject.includes('keep-me')
      })

      expect(controlTriples.length).to.be.greaterThan(0,
        'Control document triples should still exist')
    })
  })

  describe('nested subdocuments without @base', function () {
    const dbName = 'subdoc_no_base'

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)

      const schema = [
        {
          '@id': 'TopLevel',
          '@type': 'Class',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          level1: 'Level1',
        },
        {
          '@id': 'Level1',
          '@type': 'Class',
          '@subdocument': [],
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          level2: 'Level2',
        },
        {
          '@id': 'Level2',
          '@type': 'Class',
          '@subdocument': [],
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          level3: 'Level3',
        },
        {
          '@id': 'Level3',
          '@type': 'Class',
          '@subdocument': [],
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('deletes nested subdocuments without @base without leaving stray triples', async function () {
      const docToDelete = {
        '@type': 'TopLevel',
        name: 'delete-me',
        level1: {
          '@type': 'Level1',
          name: 'l1',
          level2: {
            '@type': 'Level2',
            name: 'l2',
            level3: {
              '@type': 'Level3',
              name: 'l3',
            },
          },
        },
      }

      const controlDoc = {
        '@type': 'TopLevel',
        name: 'keep-me',
        level1: {
          '@type': 'Level1',
          name: 'l1',
          level2: {
            '@type': 'Level2',
            name: 'l2',
            level3: {
              '@type': 'Level3',
              name: 'l3',
            },
          },
        },
      }

      await document.insert(agent, { instance: docToDelete })
      await document.insert(agent, { instance: controlDoc })

      const docsBefore = await document.get(agent, { query: { type: 'TopLevel', as_list: true } })
      expect(docsBefore.body).to.have.lengthOf(2)

      await document.delete(agent, { query: { id: 'TopLevel/delete-me' } })

      const docsAfter = await document.get(agent, { query: { type: 'TopLevel', as_list: true } })
      expect(docsAfter.body).to.have.lengthOf(1)
      expect(docsAfter.body[0].name).to.equal('keep-me')

      const tripleQuery = {
        '@type': 'Triple',
        subject: { '@type': 'NodeValue', variable: 'Subject' },
        predicate: { '@type': 'NodeValue', variable: 'Predicate' },
        object: { '@type': 'Value', variable: 'Object' },
      }

      const result = await woql.post(agent, tripleQuery)
      const triples = result.body.bindings

      const strayTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && subject.includes('delete-me')
      })

      expect(strayTriples).to.have.lengthOf(0,
        `Found stray triples for deleted document: ${JSON.stringify(strayTriples, null, 2)}`)

      const controlTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && subject.includes('keep-me')
      })

      expect(controlTriples.length).to.be.greaterThan(0,
        'Control document triples should still exist')
    })
  })

  describe('deeply nested subdocuments (5 levels)', function () {
    const dbName = 'subdoc_deep_nesting'

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)

      const schema = [
        {
          '@id': 'Root',
          '@type': 'Class',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          child: 'Child1',
        },
        {
          '@id': 'Child1',
          '@type': 'Class',
          '@subdocument': [],
          '@base': 'c1/',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          child: 'Child2',
        },
        {
          '@id': 'Child2',
          '@type': 'Class',
          '@subdocument': [],
          '@base': '',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          child: 'Child3',
        },
        {
          '@id': 'Child3',
          '@type': 'Class',
          '@subdocument': [],
          '@base': 'c3/',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          child: 'Child4',
        },
        {
          '@id': 'Child4',
          '@type': 'Class',
          '@subdocument': [],
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          child: 'Child5',
        },
        {
          '@id': 'Child5',
          '@type': 'Class',
          '@subdocument': [],
          '@base': '',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('deletes deeply nested subdocuments with mixed @base without leaving stray triples', async function () {
      const docToDelete = {
        '@type': 'Root',
        name: 'delete-me',
        child: {
          '@type': 'Child1',
          name: 'c1',
          child: {
            '@type': 'Child2',
            name: 'c2',
            child: {
              '@type': 'Child3',
              name: 'c3',
              child: {
                '@type': 'Child4',
                name: 'c4',
                child: {
                  '@type': 'Child5',
                  name: 'c5',
                },
              },
            },
          },
        },
      }

      const controlDoc = {
        '@type': 'Root',
        name: 'keep-me',
        child: {
          '@type': 'Child1',
          name: 'c1',
          child: {
            '@type': 'Child2',
            name: 'c2',
            child: {
              '@type': 'Child3',
              name: 'c3',
              child: {
                '@type': 'Child4',
                name: 'c4',
                child: {
                  '@type': 'Child5',
                  name: 'c5',
                },
              },
            },
          },
        },
      }

      await document.insert(agent, { instance: docToDelete })
      await document.insert(agent, { instance: controlDoc })

      const docsBefore = await document.get(agent, { query: { type: 'Root', as_list: true } })
      expect(docsBefore.body).to.have.lengthOf(2)

      await document.delete(agent, { query: { id: 'Root/delete-me' } })

      const docsAfter = await document.get(agent, { query: { type: 'Root', as_list: true } })
      expect(docsAfter.body).to.have.lengthOf(1)
      expect(docsAfter.body[0].name).to.equal('keep-me')

      const tripleQuery = {
        '@type': 'Triple',
        subject: { '@type': 'NodeValue', variable: 'Subject' },
        predicate: { '@type': 'NodeValue', variable: 'Predicate' },
        object: { '@type': 'Value', variable: 'Object' },
      }

      const result = await woql.post(agent, tripleQuery)
      const triples = result.body.bindings

      const strayTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && subject.includes('delete-me')
      })

      expect(strayTriples).to.have.lengthOf(0,
        `Found stray triples for deleted document: ${JSON.stringify(strayTriples, null, 2)}`)

      const controlTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && subject.includes('keep-me')
      })

      expect(controlTriples.length).to.be.greaterThan(0,
        'Control document triples should still exist')
    })
  })

  describe('multiple documents deletion', function () {
    const dbName = 'subdoc_multi_delete'

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)

      const schema = [
        {
          '@id': 'Parent',
          '@type': 'Class',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          child: 'Child',
        },
        {
          '@id': 'Child',
          '@type': 'Class',
          '@subdocument': [],
          '@base': 'child/',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          grandchild: 'Grandchild',
        },
        {
          '@id': 'Grandchild',
          '@type': 'Class',
          '@subdocument': [],
          '@base': '',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('deletes multiple documents with nested subdocuments without leaving stray triples', async function () {
      const doc1 = {
        '@type': 'Parent',
        name: 'doc1',
        child: {
          '@type': 'Child',
          name: 'child1',
          grandchild: {
            '@type': 'Grandchild',
            name: 'grandchild1',
          },
        },
      }

      const doc2 = {
        '@type': 'Parent',
        name: 'doc2',
        child: {
          '@type': 'Child',
          name: 'child2',
          grandchild: {
            '@type': 'Grandchild',
            name: 'grandchild2',
          },
        },
      }

      const controlDoc = {
        '@type': 'Parent',
        name: 'keep-me',
        child: {
          '@type': 'Child',
          name: 'child-keep',
          grandchild: {
            '@type': 'Grandchild',
            name: 'grandchild-keep',
          },
        },
      }

      await document.insert(agent, { instance: doc1 })
      await document.insert(agent, { instance: doc2 })
      await document.insert(agent, { instance: controlDoc })

      const docsBefore = await document.get(agent, { query: { type: 'Parent', as_list: true } })
      expect(docsBefore.body).to.have.lengthOf(3)

      await document.delete(agent, { query: { id: 'Parent/doc1' } })
      await document.delete(agent, { query: { id: 'Parent/doc2' } })

      const docsAfter = await document.get(agent, { query: { type: 'Parent', as_list: true } })
      expect(docsAfter.body).to.have.lengthOf(1)
      expect(docsAfter.body[0].name).to.equal('keep-me')

      const tripleQuery = {
        '@type': 'Triple',
        subject: { '@type': 'NodeValue', variable: 'Subject' },
        predicate: { '@type': 'NodeValue', variable: 'Predicate' },
        object: { '@type': 'Value', variable: 'Object' },
      }

      const result = await woql.post(agent, tripleQuery)
      const triples = result.body.bindings

      const strayTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && (
          subject.includes('doc1') || subject.includes('doc2')
        )
      })

      expect(strayTriples).to.have.lengthOf(0,
        `Found stray triples for deleted documents: ${JSON.stringify(strayTriples, null, 2)}`)

      const controlTriples = triples.filter(t => {
        const subject = t.Subject
        return subject && subject.includes && subject.includes('keep-me')
      })

      expect(controlTriples.length).to.be.greaterThan(0,
        'Control document triples should still exist')
    })
  })

  describe('delete with body array vs query.id parameter', function () {
    const dbName = 'subdoc_delete_body_array'

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)

      const schema = [
        {
          '@id': 'Parent',
          '@type': 'Class',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          child: 'Child',
        },
        {
          '@id': 'Child',
          '@type': 'Class',
          '@subdocument': [],
          '@key': { '@type': 'Random' },
          value: 'xsd:string',
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('delete with body array should clean up subdocuments too', async function () {
      const doc = {
        '@type': 'Parent',
        name: 'test-doc',
        child: {
          '@type': 'Child',
          value: 'child-value',
        },
      }

      await document.insert(agent, { instance: doc })

      const docsBefore = await document.get(agent, { query: { type: 'Parent', as_list: true } })
      expect(docsBefore.body).to.have.lengthOf(1)

      await document.delete(agent, { body: ['Parent/test-doc'] })

      const docsAfter = await document.get(agent, { query: { type: 'Parent', as_list: true } })
      expect(docsAfter.body).to.have.lengthOf(0)

      const tripleQuery = {
        '@type': 'Triple',
        subject: { '@type': 'NodeValue', variable: 'Subject' },
        predicate: { '@type': 'NodeValue', variable: 'Predicate' },
        object: { '@type': 'Value', variable: 'Object' },
      }

      const result = await woql.post(agent, tripleQuery)
      const triples = result.body.bindings

      expect(triples).to.have.lengthOf(0,
        `Delete with body array left ${triples.length} orphaned triples: ` +
        `${triples.slice(0, 3).map(t => t.Subject).join(', ')}`)
    })

    it('delete one doc should not affect another doc with equivalent subdocument content', async function () {
      const doc1 = {
        '@type': 'Parent',
        name: 'doc-one',
        child: {
          '@type': 'Child',
          value: 'shared-value',
        },
      }

      const doc2 = {
        '@type': 'Parent',
        name: 'doc-two',
        child: {
          '@type': 'Child',
          value: 'shared-value',
        },
      }

      await document.insert(agent, { instance: doc1 })
      await document.insert(agent, { instance: doc2 })

      const docsBefore = await document.get(agent, { query: { type: 'Parent', as_list: true } })
      expect(docsBefore.body).to.have.lengthOf(2)

      await document.delete(agent, { body: ['Parent/doc-one'] })

      const docsAfter = await document.get(agent, { query: { type: 'Parent', as_list: true } })
      expect(docsAfter.body).to.have.lengthOf(1)
      expect(docsAfter.body[0].name).to.equal('doc-two')

      const doc2Full = await document.get(agent, { query: { id: 'Parent/doc-two' } })
      expect(doc2Full.body.child).to.exist
      expect(doc2Full.body.child.value).to.equal('shared-value')
    })
  })
})
