const { expect } = require('chai')
const { Agent, db, endpoint } = require('../lib')

describe('prefixes', function () {
  describe('no-auth', function () {
    let agent

    before(function () {
      agent = new Agent()
    })

    describe('fails create with bad prefixes', function () {
      const prefixesList = [
        { '@base': 'x' },
        { '@schema': 'x' },
      ]
      for (const prefixes of prefixesList) {
        it(JSON.stringify(prefixes), async function () {
          const { path } = endpoint.db(agent.defaults())
          const r = await db.create(agent, path, { prefixes }).then(db.verifyCreateFailure)
          expect(r.body['api:error']['@type']).to.equal('api:InvalidPrefix')
          expect(r.body['api:error']['api:prefix_name']).to.equal(Object.keys(prefixes)[0])
          expect(r.body['api:error']['api:prefix_value']).to.equal(Object.values(prefixes)[0])
        })
      }
    })
  })

  describe('auth', function () {
    let agent

    before(function () {
      agent = new Agent().auth()
    })

    it('passes create with prefixes', async function () {
      const defaults = agent.defaults()
      const dbPath = endpoint.db(defaults).path
      const prefixesPath = endpoint.prefixes(defaults).path
      const prefixes = {
        '@base': 'http://a.me/b',
        '@schema': 'https://b.is/schema#',
      }
      // Create a database with prefixes
      await db.create(agent, dbPath, { prefixes }).then(db.verifyCreateSuccess)
      // Fetch the prefixes
      const r = await agent.get(prefixesPath)
      expect(r.status).to.equal(200)
      prefixes['@type'] = 'Context'
      expect(r.body).to.deep.equal(prefixes)
      // Delete the created database
      await db.del(agent, dbPath).then(db.verifyDeleteSuccess)
    })
  })
})
