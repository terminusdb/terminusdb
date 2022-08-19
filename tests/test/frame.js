const { expect } = require('chai')
const { Agent, db, document, api, util } = require('../lib')

describe('frame', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('passes frame for _system', async function () {
    const path = api.path.frameSystem()
    const r = await agent
      .post(path)
      .set('X-HTTP-Method-Override', 'GET')
      .send({ type: 'User' })
    expect(r.body).to.deep.equal({
      '@documentation': {
        '@comment': 'A database user.',
        '@properties': {
          capability: 'A set of capabilities which the user has access to.',
          key_hash: 'An optional key hash for authentication.',
          name: 'The users name.',
        },
      },
      '@key': { '@fields': ['name'], '@type': 'Lexical' },
      '@type': 'Class',
      capability: { '@class': 'Capability', '@type': 'Set' },
      key_hash: { '@class': 'xsd:string', '@type': 'Optional' },
      name: 'xsd:string',
    })
  })

  it('passes frame for _system using a real GET', async function () {
    const path = api.path.frameSystem() + '?type=User'
    const r = await agent
      .get(path)
    expect(r.body).to.deep.equal({
      '@documentation': {
        '@comment': 'A database user.',
        '@properties': {
          capability: 'A set of capabilities which the user has access to.',
          key_hash: 'An optional key hash for authentication.',
          name: 'The users name.',
        },
      },
      '@key': { '@fields': ['name'], '@type': 'Lexical' },
      '@type': 'Class',
      capability: { '@class': 'Capability', '@type': 'Set' },
      key_hash: { '@class': 'xsd:string', '@type': 'Optional' },
      name: 'xsd:string',
    })
  })

  it('passes frame for multilingual', async function () {
    const schema = {
      '@id': util.randomString(),
      '@type': 'Class',
      '@documentation': [{
        '@language': 'en',
        '@label': 'Example Class',
        '@comment': 'This is an example class',
        '@properties': { name: 'name' },
      }, {
        '@language': 'el',
        '@label': 'Παράδειγμα τάξης',
        '@comment': 'Αυτό είναι ένα παράδειγμα κλάσης',
        '@properties': { name: 'όνομα' },
      }],
      name: 'xsd:string',
    }
    await document.insert(agent, { schema })
    const path = `/api/schema/admin/${agent.dbName}?type=${schema['@id']}`
    const r = await agent
      .get(path)
    expect(r.body).to.deep.equal(
      {
        '@documentation': [
          {
            '@comment': 'This is an example class',
            '@label': 'Example Class',
            '@language': 'en',
            '@properties': { name: 'name' },
          },
          {
            '@comment': 'Αυτό είναι ένα παράδειγμα κλάσης',
            '@label': 'Παράδειγμα τάξης',
            '@language': 'el',
            '@properties': { name: 'όνομα' },
          },
        ],
        '@type': 'Class',
        name: 'xsd:string',
      })
  })
})
