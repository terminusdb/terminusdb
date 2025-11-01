const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('woql-no-schema', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent, { schema: false })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('passes AddTriple, Triple, DeleteTriple', async function () {
    const updateQuery = {
      '@type': 'AddTriple',
      subject: { '@type': 'NodeValue', node: 's' },
      predicate: { '@type': 'NodeValue', node: 'p' },
      object: { '@type': 'Value', node: 'o' },
    }
    const readQuery = {
      '@type': 'Triple',
      subject: { '@type': 'NodeValue', variable: 'S' },
      predicate: { '@type': 'NodeValue', variable: 'P' },
      object: { '@type': 'Value', variable: 'O' },
    }

    {
      // AddTriple
      const r = await woql.post(agent, updateQuery)
      expect(r.body.bindings, 'AddTriple should return one binding record').to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0], 'AddTriple should not bind any variables').to.deep.equal({})
      expect(
        r.body['api:variable_names'],
        'AddTriple should report no variable names',
      ).to.be.an('array').that.has.lengthOf(0)
      expect(r.body.deletes, 'AddTriple should not report deletes').to.equal(0)
      expect(r.body.inserts, 'AddTriple should report a single insert').to.equal(1)
      expect(
        r.body.transaction_retry_count,
        'AddTriple should not retry the transaction',
      ).to.equal(0)
    }

    {
      // Triple
      const r = await woql.post(agent, readQuery)
      expect(r.body.bindings, 'Triple read should yield one binding').to.be.an('array').that.has.lengthOf(1)
      expect(
        r.body.bindings[0],
        'Triple read should match the inserted triple',
      ).to.deep.equal({ S: 's', P: '@schema:p', O: 'o' })
      expect(
        r.body['api:variable_names'],
        'Triple read should list all projected variables',
      ).to.be.an('array').that.has.lengthOf(3)
      expect(
        r.body['api:variable_names'],
        'Triple read should expose variables S, P, and O',
      ).to.have.members(['S', 'P', 'O'])
      expect(r.body.deletes, 'Triple read should not report deletes').to.equal(0)
      expect(r.body.inserts, 'Triple read should not report inserts').to.equal(0)
      expect(
        r.body.transaction_retry_count,
        'Triple read should not retry the transaction',
      ).to.equal(0)
    }

    updateQuery['@type'] = 'DeleteTriple'

    {
      // DeleteTriple
      const r = await woql.post(agent, updateQuery)
      expect(r.body.bindings, 'DeleteTriple should return one binding record').to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0], 'DeleteTriple should not bind any variables').to.deep.equal({})
      expect(
        r.body['api:variable_names'],
        'DeleteTriple should report no variable names',
      ).to.be.an('array').that.has.lengthOf(0)
      expect(r.body.deletes, 'DeleteTriple should report one delete').to.equal(1)
      expect(r.body.inserts, 'DeleteTriple should not report inserts').to.equal(0)
      expect(
        r.body.transaction_retry_count,
        'DeleteTriple should not retry the transaction',
      ).to.equal(0)
    }
  })
})
