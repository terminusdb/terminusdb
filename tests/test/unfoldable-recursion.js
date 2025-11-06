const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('unfoldable-recursion', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  after(async function () {
    // Clean up any databases created during tests
  })

  describe('self-referencing @unfoldable documents', function () {
    it('should handle direct self-reference without crashing', async function () {
      await db.create(agent)

      try {
        // Schema with self-referencing @unfoldable class
        const schema = {
          '@type': 'Class',
          '@id': 'LinguisticObject',
          '@unfoldable': [],
          name: 'xsd:string',
          partOf: {
            '@type': 'Set',
            '@class': 'LinguisticObject',
          },
        }

        await document.insert(agent, { schema })

        // Insert a self-referencing document
        const selfRef = {
          '@type': 'LinguisticObject',
          '@id': 'LinguisticObject/self',
          name: 'Self Referencing',
          partOf: ['LinguisticObject/self'], // Points to itself
        }

        await document.insert(agent, { instance: selfRef })

        // Try to retrieve - should not crash (unfolds by default)
        const result = await document.get(agent, {
          query: {
            graph_type: 'instance',
            id: 'LinguisticObject/self',
            as_list: true,
          },
        })

        expect(result.status).to.equal(200)
        // Document should be returned (either fully expanded or with ID reference)
        expect(result.body).to.be.an('array')
        expect(result.body.length).to.be.greaterThan(0)
      } finally {
        await db.delete(agent)
      }
    })

    it('should handle circular reference (A->B->A) without crashing', async function () {
      await db.create(agent)

      try {
        const schema = {
          '@type': 'Class',
          '@id': 'Node',
          '@unfoldable': [],
          name: 'xsd:string',
          next: {
            '@type': 'Optional',
            '@class': 'Node',
          },
        }

        await document.insert(agent, { schema })

        // Create circular reference A->B->A
        const nodeA = {
          '@type': 'Node',
          '@id': 'Node/A',
          name: 'Node A',
          next: 'Node/B',
        }

        const nodeB = {
          '@type': 'Node',
          '@id': 'Node/B',
          name: 'Node B',
          next: 'Node/A', // Back to A
        }

        await document.insert(agent, { instance: [nodeA, nodeB] })

        // Retrieve with unfold - should detect cycle and not crash
        const result = await document.get(agent, {
          query: {
            graph_type: 'instance',
            id: 'Node/A',
            as_list: true,
          },
        })

        expect(result.status).to.equal(200)
        expect(result.body).to.be.an('array')
        expect(result.body.length).to.be.greaterThan(0)
      } finally {
        await db.delete(agent)
      }
    })

    it('should handle deep nested self-reference (chain of 100 nodes)', async function () {
      this.timeout(10000) // Increase timeout for this test
      await db.create(agent)

      try {
        const schema = {
          '@type': 'Class',
          '@id': 'ChainNode',
          '@unfoldable': [],
          value: 'xsd:integer',
          next: {
            '@type': 'Optional',
            '@class': 'ChainNode',
          },
        }

        await document.insert(agent, { schema })

        // Create a chain of 100 nodes
        const nodes = []
        for (let i = 0; i < 100; i++) {
          nodes.push({
            '@type': 'ChainNode',
            '@id': `ChainNode/${i}`,
            value: i,
            next: i < 99 ? `ChainNode/${i + 1}` : undefined,
          })
        }

        await document.insert(agent, { instance: nodes })

        // Retrieve head with unfold - should traverse entire chain
        const result = await document.get(agent, {
          query: {
            graph_type: 'instance',
            id: 'ChainNode/0',
            as_list: true,
          },
        })

        expect(result.status).to.equal(200)
        expect(result.body).to.be.an('array')

        // Verify we got the document (implementation may limit depth)
        expect(result.body.length).to.be.greaterThan(0)
      } finally {
        await db.delete(agent)
      }
    })

    it('should handle multiple circular paths without crashing', async function () {
      await db.create(agent)

      try {
        const schema = {
          '@type': 'Class',
          '@id': 'GraphNode',
          '@unfoldable': [],
          name: 'xsd:string',
          edges: {
            '@type': 'Set',
            '@class': 'GraphNode',
          },
        }

        await document.insert(agent, { schema })

        // Create a complex graph with multiple cycles
        // A -> B -> C -> A
        // A -> D -> A
        // B -> D
        const nodes = [
          {
            '@type': 'GraphNode',
            '@id': 'GraphNode/A',
            name: 'A',
            edges: ['GraphNode/B', 'GraphNode/D'],
          },
          {
            '@type': 'GraphNode',
            '@id': 'GraphNode/B',
            name: 'B',
            edges: ['GraphNode/C', 'GraphNode/D'],
          },
          {
            '@type': 'GraphNode',
            '@id': 'GraphNode/C',
            name: 'C',
            edges: ['GraphNode/A'],
          },
          {
            '@type': 'GraphNode',
            '@id': 'GraphNode/D',
            name: 'D',
            edges: ['GraphNode/A'],
          },
        ]

        await document.insert(agent, { instance: nodes })

        // Retrieve with unfold - should handle multiple cycles
        const result = await document.get(agent, {
          query: {
            graph_type: 'instance',
            id: 'GraphNode/A',
            as_list: true,
          },
        })

        expect(result.status).to.equal(200)
        expect(result.body).to.be.an('array')
        expect(result.body.length).to.be.greaterThan(0)
      } finally {
        await db.delete(agent)
      }
    })

    it('should handle Set with self-reference correctly', async function () {
      await db.create(agent)

      try {
        const schema = {
          '@type': 'Class',
          '@id': 'Category',
          '@unfoldable': [],
          name: 'xsd:string',
          subcategories: {
            '@type': 'Set',
            '@class': 'Category',
          },
        }

        await document.insert(agent, { schema })

        // Category that contains itself in subcategories
        const selfCategory = {
          '@type': 'Category',
          '@id': 'Category/Root',
          name: 'Root Category',
          subcategories: ['Category/Root', 'Category/Child'],
        }

        const childCategory = {
          '@type': 'Category',
          '@id': 'Category/Child',
          name: 'Child Category',
          subcategories: [],
        }

        await document.insert(agent, { instance: [selfCategory, childCategory] })

        const result = await document.get(agent, {
          query: {
            graph_type: 'instance',
            id: 'Category/Root',
            as_list: true,
          },
        })

        expect(result.status).to.equal(200)
        expect(result.body).to.be.an('array')
        expect(result.body.length).to.be.greaterThan(0)
      } finally {
        await db.delete(agent)
      }
    })
  })

  describe('verify cycle detection behavior', function () {
    it('should return @id reference for already visited nodes', async function () {
      await db.create(agent)

      try {
        const schema = {
          '@type': 'Class',
          '@id': 'RefNode',
          '@unfoldable': [],
          name: 'xsd:string',
          ref: {
            '@type': 'Optional',
            '@class': 'RefNode',
          },
        }

        await document.insert(agent, { schema })

        // A -> B -> A (circular)
        const nodes = [
          {
            '@type': 'RefNode',
            '@id': 'RefNode/A',
            name: 'Node A',
            ref: 'RefNode/B',
          },
          {
            '@type': 'RefNode',
            '@id': 'RefNode/B',
            name: 'Node B',
            ref: 'RefNode/A',
          },
        ]

        await document.insert(agent, { instance: nodes })

        const result = await document.get(agent, {
          query: {
            graph_type: 'instance',
            id: 'RefNode/A',
            as_list: true,
          },
        })

        expect(result.status).to.equal(200)
        expect(result.body).to.be.an('array')

        // The document should be returned
        // When it encounters the cycle (A->B->A), the second 'A'
        // should be represented as an ID string, not fully expanded
        const doc = result.body.find(d => d['@id'] && d['@id'].includes('RefNode/A'))
        expect(doc).to.exist
        expect(doc.name).to.equal('Node A')

        // Node B should be embedded
        if (doc.ref && typeof doc.ref === 'object') {
          expect(doc.ref['@id']).to.include('RefNode/B')
          expect(doc.ref.name).to.equal('Node B')

          // The ref from B back to A should be just an ID string (cycle detected)
          if (doc.ref.ref) {
            expect(typeof doc.ref.ref).to.equal('string')
            expect(doc.ref.ref).to.include('RefNode/A')
          }
        }
      } finally {
        await db.delete(agent)
      }
    })
  })
})
