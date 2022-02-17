const { expect } = require('chai')
const { Agent, db, endpoint, util, woql } = require('../lib')

describe('woql-queryresource', function () {
  let agent
  let path

  before(async function () {
    agent = new Agent().auth()
    path = endpoint.woqlResource(agent.defaults()).path
    await db.createAfterDel(agent, endpoint.db(agent.defaults()).path)
  })

  after(async function () {
    await db.del(agent, endpoint.db(agent.defaults()).path)
  })

  function queryTemplate () {
    return {
      query: {
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
      },
    }
  }

  it('fails with bad url', async function () {
    const query = queryTemplate()
    query.query.resource.source.url = util.randomString()
    const r = await woql.post(agent, path, query).then(woql.verifyGetFailure)
    expect(r.body['api:error']['@type']).to.equal('api:HttpRequestFailedBadUrl')
    expect(r.body['api:error']['api:url']).to.equal(query.query.resource.source.url)
  })

  it('fails with file:/// url', async function () {
    const query = queryTemplate()
    query.query.resource.source.url = 'file:///' + util.randomString()
    const r = await woql.post(agent, path, query).then(woql.verifyGetFailure)
    expect(r.body['api:error']['@type']).to.equal('api:HttpRequestFailedSocketError')
    expect(r.body['api:error']).to.have.property('api:message')
  })

  it('passes with post', async function () {
    const query = queryTemplate()
    query.query.resource.source.post = 'employees.csv'
    const r = await woql
      .multipart(agent, path, query)
      .attach('file', 'served/employees.csv')
      .then(woql.verifyGetSuccess)
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
    query.query.resource.source.post = 'employees.csv'
    const r = await woql.post(agent, path, query).then(woql.verifyGetFailure)
    expect(r.body['api:error']['@type']).to.equal('api:MissingFile')
    expect(r.body['api:error']['api:file_name']).to.equal(query.query.resource.source.post)
  })
})
