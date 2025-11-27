const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('woql-comparison', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Less Than Operator', function () {
    it('passes with 5 < 10', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 5 },
        right: { '@type': 'DataValue', data: 10 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with 21.1 < 40.1', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 21.1 },
        right: { '@type': 'DataValue', data: 40.1 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with equal values (33 < 33)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 33 },
        right: { '@type': 'DataValue', data: 33 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('fails with equal decimal values (33.0 < 33.0)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 33.0 },
        right: { '@type': 'DataValue', data: 33.0 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('passes with cross-type: 21.1 < 40', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 21.1 },
        right: { '@type': 'DataValue', data: 40 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with cross-type: 21 < 40.1', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 21 },
        right: { '@type': 'DataValue', data: 40.1 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })
  })

  describe('Greater Than Operator', function () {
    it('passes with 10 > 5', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 10 },
        right: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with 40.1 > 21.1', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 40.1 },
        right: { '@type': 'DataValue', data: 21.1 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with equal values (33 > 33) - (issue #2225)', async function () {
      // This is the core regression test for Issue #2225
      // Before fix: 33 > 33 would incorrectly return true due to structural comparison
      // After fix: correctly returns false (empty bindings)
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 33 },
        right: { '@type': 'DataValue', data: 33 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('fails with equal decimal values (33.0 > 33.0)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 33.0 },
        right: { '@type': 'DataValue', data: 33.0 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('passes with cross-type: 40.1 > 21', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 40.1 },
        right: { '@type': 'DataValue', data: 21 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with cross-type: 40 > 21.1', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 40 },
        right: { '@type': 'DataValue', data: 21.1 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with cross-type equal values: 33.0 > 33', async function () {
      // Tests that float vs integer comparison works correctly for equal values
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 33.0 },
        right: { '@type': 'DataValue', data: 33 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })
  })
})
