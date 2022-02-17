const { expect } = require('chai')
const { Agent, db, endpoint, woql } = require('../lib')

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

  it('passes with url', async function () {
    const query = queryTemplate()
    query.query.resource.source.url = 'http://127.0.0.1:7474/employees.csv'
    const r = await woql.post(agent, path, query).then(woql.verifyGetSuccess)
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

  it('fails with url and missing file', async function () {
    const query = queryTemplate()
    query.query.resource.source.url = 'http://127.0.0.1:7474/not_found.csv'
    const r = await woql.post(agent, path, query).then(woql.verifyGetFailure(r)
    expect(r.body['api:error']['@type']).to.equal('api:HttpRequestFailedFetch')
    expect(r.body['api:error']['api:url']).to.equal(query.query.resource.source.url)
  })
})
