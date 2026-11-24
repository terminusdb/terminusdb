const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('@-prefixed Properties Not Supported', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent, { label: 'Schema @-prefix Test', schema: true })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('should reject schema with @lifecycle as a property name', async function () {
    const schema = [{
      '@type': '@context',
      '@base': 'terminusdb:///data/',
      '@schema': 'terminusdb:///schema#',
    }, {
      '@id': 'TestClass',
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      name: 'xsd:string',
      '@lifecycle': 'xsd:string',
    }]

    const result = await document.insert(agent, { schema, fullReplace: true }).unverified()

    expect(result.status).to.not.equal(500)
    expect(result.status).to.be.oneOf([400, 422])
    expect(result.body).to.have.property('api:message')
    expect(result.body['api:message']).to.match(/@.*not supported/i)
  })

  it('should reject schema with @version as a property name', async function () {
    const schema = [{
      '@type': '@context',
      '@base': 'terminusdb:///data/',
      '@schema': 'terminusdb:///schema#',
    }, {
      '@id': 'Product',
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      name: 'xsd:string',
      '@version': 'xsd:string',
    }]

    const result = await document.insert(agent, { schema, fullReplace: true }).unverified()

    expect(result.status).to.not.equal(500)
    expect(result.status).to.be.oneOf([400, 422])
    expect(result.body).to.have.property('api:message')
    expect(result.body['api:message']).to.match(/@.*not supported/i)
  })

  it('should reject schema with any @-prefixed property name', async function () {
    const schema = [{
      '@type': '@context',
      '@base': 'terminusdb:///data/',
      '@schema': 'terminusdb:///schema#',
    }, {
      '@id': 'CustomClass',
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      status: 'xsd:string',
      '@customField': 'xsd:string',
    }]

    const result = await document.insert(agent, { schema, fullReplace: true }).unverified()

    expect(result.status).to.not.equal(500)
    expect(result.status).to.be.oneOf([400, 422])
    expect(result.body).to.have.property('api:message')
    expect(result.body['api:message']).to.match(/@.*not supported/i)
  })
})
