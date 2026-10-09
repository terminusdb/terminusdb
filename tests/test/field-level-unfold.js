'use strict'

const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('Field-Level Unfold', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  describe('Schema Definition', function () {
    before(async function () {
      agent.dbName = 'field_unfold_schema_test'
      await db.create(agent, { label: 'Field Unfold Schema Test' })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should accept @unfold annotation on Optional property', async function () {
      const schema = [
        {
          '@type': 'Class',
          '@id': 'Customer',
          name: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Order',
          customer: {
            '@type': 'Optional',
            '@class': 'Customer',
            '@unfold': true,
          },
        },
      ]

      const r = await document.insert(agent, { schema })
      expect(r.status).to.equal(200)
    })

    it('should preserve @unfold in schema roundtrip', async function () {
      const r = await document.get(agent, { query: { graph_type: 'schema', id: 'Order' } })
      expect(r.status).to.equal(200)
      expect(r.body.customer['@unfold']).to.equal(true)
    })

    it('should accept @unfold on Set property', async function () {
      const schema = [
        {
          '@type': 'Class',
          '@id': 'Product',
          name: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Cart',
          products: {
            '@type': 'Set',
            '@class': 'Product',
            '@unfold': true,
          },
        },
      ]

      const r = await document.insert(agent, { schema })
      expect(r.status).to.equal(200)

      const r2 = await document.get(agent, { query: { graph_type: 'schema', id: 'Cart' } })
      expect(r2.status).to.equal(200)
      expect(r2.body.products['@unfold']).to.equal(true)
    })

    it('should accept @unfold on Array property', async function () {
      const schema = [
        {
          '@type': 'Class',
          '@id': 'Item',
          value: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Container',
          items: {
            '@type': 'Array',
            '@class': 'Item',
            '@unfold': true,
          },
        },
      ]

      const r = await document.insert(agent, { schema })
      expect(r.status).to.equal(200)

      const r2 = await document.get(agent, { query: { graph_type: 'schema', id: 'Container' } })
      expect(r2.status).to.equal(200)
      expect(r2.body.items['@unfold']).to.equal(true)
    })

    it('should accept @unfold on List property', async function () {
      const schema = [
        {
          '@type': 'Class',
          '@id': 'Element',
          data: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Sequence',
          elements: {
            '@type': 'List',
            '@class': 'Element',
            '@unfold': true,
          },
        },
      ]

      const r = await document.insert(agent, { schema })
      expect(r.status).to.equal(200)

      const r2 = await document.get(agent, { query: { graph_type: 'schema', id: 'Sequence' } })
      expect(r2.status).to.equal(200)
      expect(r2.body.elements['@unfold']).to.equal(true)
    })
  })

  describe('Document Retrieval with @unfold', function () {
    before(async function () {
      agent.dbName = 'field_unfold_retrieval_test'
      await db.create(agent, { label: 'Field Unfold Retrieval Test' })

      const schema = [
        {
          '@type': 'Class',
          '@id': 'Address',
          street: 'xsd:string',
          city: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Person',
          name: 'xsd:string',
          address: {
            '@type': 'Optional',
            '@class': 'Address',
            '@unfold': true,
          },
          friend: {
            '@type': 'Optional',
            '@class': 'Person',
          },
        },
      ]

      await document.insert(agent, { schema })

      const instances = [
        {
          '@type': 'Address',
          '@id': 'Address/home',
          street: '123 Main St',
          city: 'Springfield',
        },
        {
          '@type': 'Person',
          '@id': 'Person/alice',
          name: 'Alice',
          address: 'Address/home',
        },
        {
          '@type': 'Person',
          '@id': 'Person/bob',
          name: 'Bob',
          friend: 'Person/alice',
        },
      ]

      await document.insert(agent, { instance: instances })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should unfold property with @unfold: true when unfold=true', async function () {
      const r = await document.get(agent, { queryString: 'id=Person/alice&unfold=true' })
      expect(r.status).to.equal(200)
      expect(r.body.address).to.be.an('object')
      expect(r.body.address['@id']).to.equal('Address/home')
      expect(r.body.address.street).to.equal('123 Main St')
      expect(r.body.address.city).to.equal('Springfield')
    })

    it('should NOT unfold property without @unfold when unfold=true', async function () {
      const r = await document.get(agent, { queryString: 'id=Person/bob&unfold=true' })
      expect(r.status).to.equal(200)
      expect(r.body.friend).to.equal('Person/alice')
    })

    it('should NOT unfold property with @unfold when unfold=false', async function () {
      const r = await document.get(agent, { queryString: 'id=Person/alice&unfold=false' })
      expect(r.status).to.equal(200)
      expect(r.body.address).to.equal('Address/home')
    })
  })

  describe('List Property Retrieval with @unfold', function () {
    before(async function () {
      agent.dbName = 'field_unfold_list_retrieval_test'
      await db.create(agent, { label: 'Field Unfold List Retrieval Test' })

      const schema = [
        {
          '@type': 'Class',
          '@id': 'Element',
          data: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Sequence',
          elements: {
            '@type': 'List',
            '@class': 'Element',
            '@unfold': true,
          },
        },
      ]

      await document.insert(agent, { schema })

      const instances = [
        { '@type': 'Element', '@id': 'Element/e1', data: 'first' },
        { '@type': 'Element', '@id': 'Element/e2', data: 'second' },
        {
          '@type': 'Sequence',
          '@id': 'Sequence/s1',
          elements: ['Element/e1', 'Element/e2'],
        },
      ]

      await document.insert(agent, { instance: instances })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should unfold List property with @unfold: true when unfold=true', async function () {
      const r = await document.get(agent, { queryString: 'id=Sequence/s1&unfold=true' })
      expect(r.status).to.equal(200)
      expect(r.body.elements).to.be.an('array').with.lengthOf(2)
      expect(r.body.elements[0]).to.be.an('object')
      expect(r.body.elements[0]['@id']).to.equal('Element/e1')
      expect(r.body.elements[0].data).to.equal('first')
      expect(r.body.elements[1]['@id']).to.equal('Element/e2')
      expect(r.body.elements[1].data).to.equal('second')
    })

    it('should NOT unfold List property when unfold=false', async function () {
      const r = await document.get(agent, { queryString: 'id=Sequence/s1&unfold=false' })
      expect(r.status).to.equal(200)
      expect(r.body.elements).to.deep.equal(['Element/e1', 'Element/e2'])
    })
  })

  describe('2D Array Property Retrieval with @unfold', function () {
    before(async function () {
      agent.dbName = 'field_unfold_array2d_retrieval_test'
      await db.create(agent, { label: 'Field Unfold 2D Array Retrieval Test' })

      const schema = [
        {
          '@type': 'Class',
          '@id': 'Cell',
          data: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Grid',
          cells: {
            '@type': 'Array',
            '@dimensions': 2,
            '@class': 'Cell',
            '@unfold': true,
          },
        },
      ]

      await document.insert(agent, { schema })

      const instances = [
        { '@type': 'Cell', '@id': 'Cell/c00', data: '00' },
        { '@type': 'Cell', '@id': 'Cell/c01', data: '01' },
        { '@type': 'Cell', '@id': 'Cell/c10', data: '10' },
        { '@type': 'Cell', '@id': 'Cell/c11', data: '11' },
        {
          '@type': 'Grid',
          '@id': 'Grid/g1',
          cells: [
            ['Cell/c00', 'Cell/c01'],
            ['Cell/c10', 'Cell/c11'],
          ],
        },
      ]

      await document.insert(agent, { instance: instances })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should unfold 2D Array property with @unfold: true when unfold=true', async function () {
      const r = await document.get(agent, { queryString: 'id=Grid/g1&unfold=true' })
      expect(r.status).to.equal(200)
      expect(r.body.cells).to.be.an('array').with.lengthOf(2)
      expect(r.body.cells[0][0]).to.be.an('object')
      expect(r.body.cells[0][0]['@id']).to.equal('Cell/c00')
      expect(r.body.cells[0][0].data).to.equal('00')
      expect(r.body.cells[0][1]['@id']).to.equal('Cell/c01')
      expect(r.body.cells[1][0]['@id']).to.equal('Cell/c10')
      expect(r.body.cells[1][1]['@id']).to.equal('Cell/c11')
      expect(r.body.cells[1][1].data).to.equal('11')
    })

    it('should NOT unfold 2D Array property when unfold=false', async function () {
      const r = await document.get(agent, { queryString: 'id=Grid/g1&unfold=false' })
      expect(r.status).to.equal(200)
      expect(r.body.cells).to.deep.equal([
        ['Cell/c00', 'Cell/c01'],
        ['Cell/c10', 'Cell/c11'],
      ])
    })
  })

  describe('Interaction with class-level @unfoldable', function () {
    before(async function () {
      agent.dbName = 'field_unfold_interaction_test'
      await db.create(agent, { label: 'Field Unfold Interaction Test' })

      const schema = [
        {
          '@type': 'Class',
          '@id': 'UnfoldableClass',
          '@unfoldable': [],
          data: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'RegularClass',
          value: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'TestClass',
          unfoldableRef: {
            '@type': 'Optional',
            '@class': 'UnfoldableClass',
          },
          regularWithUnfold: {
            '@type': 'Optional',
            '@class': 'RegularClass',
            '@unfold': true,
          },
          regularWithoutUnfold: {
            '@type': 'Optional',
            '@class': 'RegularClass',
          },
        },
      ]

      await document.insert(agent, { schema })

      const instances = [
        {
          '@type': 'UnfoldableClass',
          '@id': 'UnfoldableClass/u1',
          data: 'unfoldable data',
        },
        {
          '@type': 'RegularClass',
          '@id': 'RegularClass/r1',
          value: 'regular value 1',
        },
        {
          '@type': 'RegularClass',
          '@id': 'RegularClass/r2',
          value: 'regular value 2',
        },
        {
          '@type': 'TestClass',
          '@id': 'TestClass/test1',
          unfoldableRef: 'UnfoldableClass/u1',
          regularWithUnfold: 'RegularClass/r1',
          regularWithoutUnfold: 'RegularClass/r2',
        },
      ]

      await document.insert(agent, { instance: instances })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should unfold @unfoldable class even without @unfold on property', async function () {
      const r = await document.get(agent, { queryString: 'id=TestClass/test1&unfold=true' })
      expect(r.status).to.equal(200)
      expect(r.body.unfoldableRef).to.be.an('object')
      expect(r.body.unfoldableRef.data).to.equal('unfoldable data')
    })

    it('should unfold regular class with @unfold: true', async function () {
      const r = await document.get(agent, { queryString: 'id=TestClass/test1&unfold=true' })
      expect(r.status).to.equal(200)
      expect(r.body.regularWithUnfold).to.be.an('object')
      expect(r.body.regularWithUnfold.value).to.equal('regular value 1')
    })

    it('should NOT unfold regular class without @unfold', async function () {
      const r = await document.get(agent, { queryString: 'id=TestClass/test1&unfold=true' })
      expect(r.status).to.equal(200)
      expect(r.body.regularWithoutUnfold).to.equal('RegularClass/r2')
    })
  })

  describe('Cycle Handling with @unfold', function () {
    before(async function () {
      agent.dbName = 'field_unfold_cycle_test'
      await db.create(agent, { label: 'Field Unfold Cycle Test' })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should accept schema with @unfold cycle (consistent with @unfoldable)', async function () {
      // Schema cycles are allowed - runtime visited-node tracking prevents infinite loops
      const schema = [
        {
          '@type': 'Class',
          '@id': 'CycleA',
          name: 'xsd:string',
          toB: {
            '@type': 'Optional',
            '@class': 'CycleB',
            '@unfold': true,
          },
        },
        {
          '@type': 'Class',
          '@id': 'CycleB',
          name: 'xsd:string',
          toA: {
            '@type': 'Optional',
            '@class': 'CycleA',
            '@unfold': true,
          },
        },
      ]

      await document.insert(agent, { schema })

      // Insert cyclic data
      const instances = [
        { '@type': 'CycleA', '@id': 'CycleA/a1', name: 'A1', toB: 'CycleB/b1' },
        { '@type': 'CycleB', '@id': 'CycleB/b1', name: 'B1', toA: 'CycleA/a1' },
      ]
      await document.insert(agent, { instance: instances })

      // Retrieve with unfold - should return @id for visited nodes (not infinite loop)
      const r = await document.get(agent, { queryString: 'id=CycleA/a1&unfold=true' })
      expect(r.status).to.equal(200)
      expect(r.body.toB).to.be.an('object')
      expect(r.body.toB.name).to.equal('B1')
      // The back-reference should be just an @id (already visited)
      expect(r.body.toB.toA).to.equal('CycleA/a1')
    })
  })
})
