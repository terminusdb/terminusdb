const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('subdoc-content-addressed-deletion', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  describe('shared content-addressed subdocuments', function () {
    const dbName = 'content_addressed_subdoc_deletion'

    const schema = [
      {
        '@type': 'Class',
        '@key': { '@type': 'Lexical', '@fields': ['order-number'] },
        '@id': 'PurchaseOrder',
        '@base': 'purchase-order/',
        'purchase-order-line-item': { '@class': 'PurchaseOrderLineItem', '@type': 'Set' },
        'order-number': 'xsd:string',
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'PurchaseOrderLineItem',
        '@base': 'position/',
        '@subdocument': [],
        position: { '@class': 'xsd:integer', '@type': 'Optional' },
        metadata: { '@class': 'Metadata', '@type': 'Optional' },
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'Metadata',
        '@base': 'id/',
        '@subdocument': [],
        vendor: { '@class': 'Vendor', '@type': 'Optional' },
        'term-start': { '@class': 'TermStart', '@type': 'Optional' },
        'term-end': { '@class': 'TermEnd', '@type': 'Optional' },
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'Vendor',
        '@base': 'id/',
        '@subdocument': [],
        'vendor-name': { '@class': 'xsd:string', '@type': 'Optional' },
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'TermStart',
        '@base': 'term-start/',
        '@subdocument': [],
        date: { '@class': 'xsd:date', '@type': 'Optional' },
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'TermEnd',
        '@base': 'id/',
        '@subdocument': [],
        date: { '@class': 'xsd:date', '@type': 'Optional' },
      },
    ]

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)
      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('handles deletion when multiple line items share identical subdocuments', async function () {
      this.timeout(30000)

      const purchaseOrder = {
        '@type': 'PurchaseOrder',
        'order-number': 'SHARED-001',
        'purchase-order-line-item': [
          {
            '@type': 'PurchaseOrderLineItem',
            position: 1,
            metadata: {
              '@type': 'Metadata',
              vendor: { '@type': 'Vendor', 'vendor-name': 'SoftwareOne Deutschland GmbH' },
              'term-start': { '@type': 'TermStart', date: '2025-01-01' },
              'term-end': { '@type': 'TermEnd', date: '2029-04-01' },
            },
          },
          {
            '@type': 'PurchaseOrderLineItem',
            position: 2,
            metadata: {
              '@type': 'Metadata',
              vendor: { '@type': 'Vendor', 'vendor-name': 'SoftwareOne Deutschland GmbH' },
              'term-start': { '@type': 'TermStart', date: '2025-01-01' },
              'term-end': { '@type': 'TermEnd', date: '2029-04-01' },
            },
          },
          {
            '@type': 'PurchaseOrderLineItem',
            position: 3,
            metadata: {
              '@type': 'Metadata',
              vendor: { '@type': 'Vendor', 'vendor-name': 'SoftwareOne Deutschland GmbH' },
              'term-start': { '@type': 'TermStart', date: '2025-01-01' },
              'term-end': { '@type': 'TermEnd', date: '2029-04-01' },
            },
          },
        ],
      }

      await document.insert(agent, { instance: purchaseOrder })

      await document.delete(agent, { query: { id: 'purchase-order/SHARED-001' } })

      const tripleQuery = {
        '@type': 'Triple',
        subject: { '@type': 'NodeValue', variable: 'Subject' },
        predicate: { '@type': 'NodeValue', variable: 'Predicate' },
        object: { '@type': 'Value', variable: 'Object' },
      }
      const result = await woql.post(agent, tripleQuery)
      const triples = result.body.bindings

      expect(triples).to.have.lengthOf(0,
        `Found ${triples.length} orphaned triples after deletion`)
    })
  })

  describe('two documents with shared subdocument content - delete one', function () {
    const dbName = 'two_docs_shared_subdoc'

    const schema = [
      {
        '@type': 'Class',
        '@key': { '@type': 'Lexical', '@fields': ['order-number'] },
        '@id': 'PurchaseOrder',
        '@base': 'purchase-order/',
        'line-items': { '@class': 'LineItem', '@type': 'Set' },
        'order-number': 'xsd:string',
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'LineItem',
        '@base': 'line/',
        '@subdocument': [],
        position: { '@class': 'xsd:integer', '@type': 'Optional' },
        vendor: { '@class': 'Vendor', '@type': 'Optional' },
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'Vendor',
        '@base': 'vendor/',
        '@subdocument': [],
        name: { '@class': 'xsd:string', '@type': 'Optional' },
      },
    ]

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)
      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('deleting doc1 should not affect doc2 subdocuments with same content', async function () {
      this.timeout(30000)

      const doc1 = {
        '@type': 'PurchaseOrder',
        'order-number': 'DOC-001',
        'line-items': [
          {
            '@type': 'LineItem',
            position: 1,
            vendor: { '@type': 'Vendor', name: 'Shared Vendor' },
          },
          {
            '@type': 'LineItem',
            position: 2,
            vendor: { '@type': 'Vendor', name: 'Shared Vendor' },
          },
        ],
      }

      const doc2 = {
        '@type': 'PurchaseOrder',
        'order-number': 'DOC-002',
        'line-items': [
          {
            '@type': 'LineItem',
            position: 1,
            vendor: { '@type': 'Vendor', name: 'Shared Vendor' },
          },
          {
            '@type': 'LineItem',
            position: 2,
            vendor: { '@type': 'Vendor', name: 'Shared Vendor' },
          },
        ],
      }

      await document.insert(agent, { instance: doc1 })
      await document.insert(agent, { instance: doc2 })

      await document.delete(agent, { query: { id: 'purchase-order/DOC-001' } })

      const docsAfter = await document.get(agent, { query: { type: 'PurchaseOrder', as_list: true } })
      expect(docsAfter.body).to.have.lengthOf(1)
      expect(docsAfter.body[0]['order-number']).to.equal('DOC-002')

      const doc2Full = await document.get(agent, { query: { id: 'purchase-order/DOC-002' } })

      expect(doc2Full.body['line-items']).to.have.lengthOf(2)
      expect(doc2Full.body['line-items'][0].vendor).to.exist
      expect(doc2Full.body['line-items'][0].vendor.name).to.equal('Shared Vendor')
    })

    it('deleting doc2 should leave zero triples', async function () {
      await document.delete(agent, { query: { id: 'purchase-order/DOC-002' } })

      const tripleQuery = {
        '@type': 'Triple',
        subject: { '@type': 'NodeValue', variable: 'Subject' },
        predicate: { '@type': 'NodeValue', variable: 'Predicate' },
        object: { '@type': 'Value', variable: 'Object' },
      }

      const result = await woql.post(agent, tripleQuery)
      const triples = result.body.bindings

      expect(triples).to.have.lengthOf(0,
        `Found ${triples.length} orphaned triples after deleting both documents`)
    })
  })

  describe('bulk delete of documents with shared subdocuments', function () {
    const dbName = 'bulk_delete_shared_subdoc'

    const schema = [
      {
        '@type': 'Class',
        '@key': { '@type': 'Lexical', '@fields': ['name'] },
        '@id': 'Parent',
        '@base': 'parent/',
        name: 'xsd:string',
        child: { '@class': 'Child', '@type': 'Optional' },
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'Child',
        '@base': 'child/',
        '@subdocument': [],
        value: { '@class': 'xsd:string', '@type': 'Optional' },
        nested: { '@class': 'Nested', '@type': 'Optional' },
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'Nested',
        '@base': 'nested/',
        '@subdocument': [],
        data: { '@class': 'xsd:string', '@type': 'Optional' },
      },
    ]

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)
      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('bulk deletion of 3 documents with identical subdocument content', async function () {
      this.timeout(30000)

      const docs = [
        {
          '@type': 'Parent',
          name: 'doc1',
          child: {
            '@type': 'Child',
            value: 'shared-value',
            nested: { '@type': 'Nested', data: 'shared-nested-data' },
          },
        },
        {
          '@type': 'Parent',
          name: 'doc2',
          child: {
            '@type': 'Child',
            value: 'shared-value',
            nested: { '@type': 'Nested', data: 'shared-nested-data' },
          },
        },
        {
          '@type': 'Parent',
          name: 'doc3',
          child: {
            '@type': 'Child',
            value: 'shared-value',
            nested: { '@type': 'Nested', data: 'shared-nested-data' },
          },
        },
      ]

      for (const doc of docs) {
        await document.insert(agent, { instance: doc })
      }

      await document.delete(agent, { query: { id: 'parent/doc1' } })
      await document.delete(agent, { query: { id: 'parent/doc2' } })
      await document.delete(agent, { query: { id: 'parent/doc3' } })

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
        `Found ${triples.length} orphaned triples after bulk deletion`)
    })
  })
})
