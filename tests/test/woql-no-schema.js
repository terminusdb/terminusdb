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
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0]).to.deep.equal({})
      expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
      expect(r.body.deletes).to.equal(0)
      expect(r.body.inserts).to.equal(1)
      expect(r.body.transaction_retry_count).to.equal(0)
    }

    {
      // Triple
      const r = await woql.post(agent, readQuery)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0]).to.deep.equal({ S: 's', P: '@schema:p', O: 'o' })
      expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(3)
      expect(r.body['api:variable_names']).to.have.members(['S', 'P', 'O'])
      expect(r.body.deletes).to.equal(0)
      expect(r.body.inserts).to.equal(0)
      expect(r.body.transaction_retry_count).to.equal(0)
    }

    updateQuery['@type'] = 'DeleteTriple'

    {
      // DeleteTriple
      const r = await woql.post(agent, updateQuery)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0]).to.deep.equal({})
      expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(0)
      expect(r.body.deletes).to.equal(1)
      expect(r.body.inserts).to.equal(0)
      expect(r.body.transaction_retry_count).to.equal(0)
    }
  })
})
