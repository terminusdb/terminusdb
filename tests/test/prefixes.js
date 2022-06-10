const { expect } = require('chai')
const { Agent, api, db } = require('../lib')

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
          await db
            .create(agent, { prefixes })
            .fails(api.error.invalidPrefix(Object.keys(prefixes)[0], Object.values(prefixes)[0]))
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
      const prefixes = {
        '@base': 'http://a.me/b',
        '@schema': 'https://b.is/schema#',
      }
      // Create a database with prefixes
      await db.create(agent, { prefixes })
      // Fetch the prefixes
      const r = await agent.get(api.path.prefixes(agent))
      expect(r.status).to.equal(200)
      prefixes['@type'] = 'Context'
      expect(r.body).to.deep.equal(prefixes)
      // Delete the created database
      await db.delete(agent)
    })
  })
})
