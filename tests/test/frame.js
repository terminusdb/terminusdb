const { expect } = require('chai')
const { Agent, api } = require('../lib')

describe('frame', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
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

})
