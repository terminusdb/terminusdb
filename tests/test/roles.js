const { expect } = require('chai')
const { Agent, util } = require('../lib')

describe('roles', function () {
  it('passes add', async function () {
    const agent = new Agent().auth()
    const roleName = util.randomString()
    const result = await agent
      .post('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access',
          'meta_write_access'],
      })
    expect(result.body).to.equal(`terminusdb://system/data/Role/${roleName}`)
    expect(result.status).to.equal(200)
  })

  it('passes delete', async function () {
    const agent = new Agent().auth()
    const roleName = util.randomString()
    await agent
      .post('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access',
          'meta_write_access'],
      })

    const result = await agent.delete(`/api/roles/${roleName}`)
    expect(result.body).to.deep.equal({ '@type': 'api:RolesResponse', 'api:status': 'api:success' })
    expect(result.status).to.equal(200)
  })

  it('passes update', async function () {
    const agent = new Agent().auth()
    const roleName = util.randomString()
    await agent
      .post('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access',
          'meta_write_access'],
      })

    const resultGet1 = await agent
      .get(`/api/roles/${roleName}`)
    expect(resultGet1.status).to.equal(200)
    expect(resultGet1.body.action).to.deep.equal(['meta_read_access',
      'meta_write_access'])

    const resultPut = await agent
      .put('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access'],
      })
    expect(resultPut.status).to.equal(200)

    const resultGet2 = await agent
      .get(`/api/roles/${roleName}`)
    expect(resultGet2.status).to.equal(200)
    expect(resultGet2.body.action).to.deep.equal(['meta_read_access'])
  })
})
