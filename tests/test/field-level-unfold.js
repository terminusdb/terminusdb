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

  describe('Cycle Detection with @unfold', function () {
    before(async function () {
      agent.dbName = 'field_unfold_cycle_test'
      await db.create(agent, { label: 'Field Unfold Cycle Test' })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should reject schema with @unfold cycle', async function () {
      const schema = [
        {
          '@type': 'Class',
          '@id': 'CycleA',
          toB: {
            '@type': 'Optional',
            '@class': 'CycleB',
            '@unfold': true,
          },
        },
        {
          '@type': 'Class',
          '@id': 'CycleB',
          toA: {
            '@type': 'Optional',
            '@class': 'CycleA',
            '@unfold': true,
          },
        },
      ]

      const r = await document.insert(agent, { schema }).unverified()
      expect(r.status).to.equal(400)
    })
  })
})
