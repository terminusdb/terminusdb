const { expect } = require('chai')
const { Agent, api, db, util, woql } = require('../lib')

describe('woql-queryresource', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  function queryTemplate () {
    return {
      '@type': 'Get',
      columns: [
        {
          '@type': 'Column',
          indicator: { '@type': 'Indicator', name: 'Name' },
          variable: 'Name',
        },
      ],
      resource: {
        '@type': 'QueryResource',
        source: { '@type': 'Source' },
        format: 'csv',
      },
    }
  }

  it('fails with bad url', async function () {
    const query = queryTemplate()
    query.resource.source.url = util.randomString()
    await woql
      .post(agent, query)
      .fails(api.error.httpRequestFailedBadUrl(query.resource.source.url))
  })

  it('fails with file:/// url', async function () {
    const query = queryTemplate()
    query.resource.source.url = 'file:///' + util.randomString()
    await woql
      .post(agent, query)
      .fails(api.error.httpRequestFailedSocketError(query.resource.source.url))
  })

  it('passes with post', async function () {
    const query = queryTemplate()
    query.resource.source.post = 'employees.csv'
    const r = await woql
      .multipart(agent, query)
      .attach('file', 'served/employees.csv')
    expect(r.body['api:variable_names']).to.be.an('array').that.has.lengthOf(1)
    expect(r.body['api:variable_names'][0]).to.equal('Name')
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(4)
    expect(r.body.bindings).to.deep.equal([
      { Name: { '@type': 'xsd:string', '@value': 'Destiny Norris' } },
      { Name: { '@type': 'xsd:string', '@value': 'Darci Prosser' } },
      { Name: { '@type': 'xsd:string', '@value': 'Alanah Bloggs' } },
      { Name: { '@type': 'xsd:string', '@value': 'Fabian Dalby' } },
    ])
    expect(r.body.deletes).to.equal(0)
    expect(r.body.inserts).to.equal(0)
    expect(r.body.transaction_retry_count).to.equal(0)
  })

  it('fails with post and missing file', async function () {
    const query = queryTemplate()
    query.resource.source.post = 'employees.csv'
    await woql
      .post(agent, query)
      .fails(api.error.missingFile(query.resource.source.post))
  })
})
