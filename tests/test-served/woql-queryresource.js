const { expect } = require('chai')
const { Agent, api, db, woql } = require('../lib')

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

  it('passes with url', async function () {
    const query = queryTemplate()
    query.resource.source.url = 'http://127.0.0.1:7474/employees.csv'
    const r = await woql.post(agent, query)
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
    query.resource.source.url = 'http://127.0.0.1:7474/not_found.csv'
    await woql
      .post(agent, query)
      .fails(api.error.httpRequestFailedFetch(query.resource.source.url))
  })
})
