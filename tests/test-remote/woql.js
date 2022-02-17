const { expect } = require('chai')
const { Agent, db, endpoint, woql } = require('../lib')

describe('woql', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    const { path } = endpoint.db(agent.defaults())
    await db.createAfterDel(agent, path)
  })

  after(async function () {
    const { path } = endpoint.db(agent.defaults())
    await db.del(agent, path)
  })

  it('passes QueryResource with url', async function () {
    const query = {
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
          source: { '@type': 'Source', url: 'http://127.0.0.1:7474/employees.csv' },
          format: 'csv',
        },
      },
    }

    const { path } = endpoint.woqlResource(agent.defaults())
    const r = await woql.post(agent, path, query)
    console.error(r.body)
    woql.verifyGetSuccess(r)

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

  it('fails QueryResource with url and missing file', async function () {
    const query = {
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
          source: { '@type': 'Source', url: 'http://127.0.0.1:7474/missing.csv' },
          format: 'csv',
        },
      },
    }

    const { path } = endpoint.woqlResource(agent.defaults())
    const r = await woql.post(agent, path, query)
    console.error(r.body)
    woql.verifyGetFailure(r)
    expect(r.body['api:error']['@type']).to.equal('api:HttpRequestFailedFetch')
    expect(r.body['api:error']['api:url']).to.equal(query.query.resource.source.url)
  })
})
