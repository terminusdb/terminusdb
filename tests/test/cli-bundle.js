const { expect } = require('chai')

const { Agent, Cli, util } = require('../lib')

describe('cli-bundle', function () {
  let agent
  let cli

  before(async function () {
    agent = new Agent()
    cli = new Cli(/* { debugCommand: true, debugOutput: true } */)
    await cli.store.init()
  })

  it('passes bundle, unbundle empty database', async function () {
    await cli.db.create(agent)
    await cli.bundle(agent)
    await cli.unbundle(agent)
    await cli.db.delete(agent)
  })

  it('passes bundle, unbundle database with doc', async function () {
    this.timeout(200000)
    await cli.db.create(agent)
    const schema = { '@type': 'Class', '@id': util.randomString() }
    await cli.doc.insert(agent, schema, { graphType: 'schema' })
    await cli.bundle(agent)
    await cli.unbundle(agent)
    const r = await cli.doc.get(agent, { graphType: 'schema' })
    expect(r[0]).to.deep.equal(util.defaultContext)
    expect(r[1]).to.deep.equal(schema)
    await cli.db.delete(agent)
  })

  describe('bundle, unbundle into a different store', function () {
    // A separate Cli instance gets its own store path via
    // TERMINUSDB_SERVER_DB_PATH, so cli and cli2 are independent stores.
    let cli2

    before(async function () {
      cli2 = new Cli()
      await cli2.store.init()
    })

    it('passes', async function () {
      this.timeout(200000)
      const schema = {
        '@type': 'Class',
        '@id': util.randomString(),
        '@key': { '@type': 'Random' },
        name: 'xsd:string',
      }
      await cli.db.create(agent)
      await cli.doc.insert(agent, schema, { graphType: 'schema' })
      await cli.doc.insert(agent, { '@type': schema['@id'], name: 'Test1' })
      await cli.bundle(agent)
      await cli2.db.create(agent)
      await cli2.unbundle(agent)
      const r = await cli2.doc.get(agent, { graphType: 'schema' })
      expect(r[0]).to.deep.equal(util.defaultContext)
      expect(r[1]).to.deep.equal(schema)
      await cli.db.delete(agent)
      await cli2.db.delete(agent)
    })
  })

  describe('bundle, unbundle different empty database', function () {
    before(async function () {
      await cli.db.create(agent)
    })

    after(async function () {
      await cli.db.delete(agent)
    })

    it('passes', async function () {
      this.timeout(200000)
      const schema = { '@type': 'Class', '@id': util.randomString() }
      await cli.doc.insert(agent, schema, { graphType: 'schema' })
      await cli.bundle(agent)
      await cli.db.delete(agent)
      await cli.db.create(agent)
      await cli.unbundle(agent)
      const r = await cli.doc.get(agent, { graphType: 'schema' })
      expect(r[0]).to.deep.equal(util.defaultContext)
      expect(r[1]).to.deep.equal(schema)
    })
  })
})
