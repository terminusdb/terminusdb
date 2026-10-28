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

  it('passes frame for a diamond with an abstract range', async function () {
    // A subclass narrows an inherited Set of an abstract class to an
    // Optional of a more specific abstract class. The frame expands the
    // range to concrete subclasses; the diamond check must still compare
    // the declared types.
    const suffix = util.randomString()
    const schema = [
      {
        '@id': `DObject${suffix}`,
        '@type': 'Class',
        '@abstract': [],
      },
      {
        '@id': `DAgent${suffix}`,
        '@type': 'Class',
        '@abstract': [],
        '@inherits': `DObject${suffix}`,
      },
      {
        '@id': `DXRobot${suffix}`,
        '@type': 'Class',
        '@inherits': `DAgent${suffix}`,
      },
      {
        '@id': `DEvent${suffix}`,
        '@type': 'Class',
        hasParticipant: { '@type': 'Set', '@class': `DObject${suffix}` },
      },
      {
        '@id': `DAction${suffix}`,
        '@type': 'Class',
        '@inherits': `DEvent${suffix}`,
        hasParticipant: { '@type': 'Optional', '@class': `DAgent${suffix}` },
      },
    ]
    await document.insert(agent, { schema })
    const path = `/api/schema/admin/${agent.dbName}?type=DAction${suffix}`
    const r = await agent.get(path)
    expect(r.body.hasParticipant).to.deep.equal({
      '@type': 'Optional',
      '@class': [`DXRobot${suffix}`],
    })
    const all = await agent.get(`/api/schema/admin/${agent.dbName}`)
    expect(all.body[`DAction${suffix}`].hasParticipant).to.deep.equal({
      '@type': 'Optional',
      '@class': [`DXRobot${suffix}`],
    })
  })

  it('passes frame for a diamond narrowed to a required value', async function () {
    // A subclass narrows an inherited Set of an abstract class to a
    // mandatory (exactly-one) value of a more specific abstract class.
    // Required is a subset of Set, so the narrower declaration wins and
    // the frame expands the abstract range to concrete subclasses.
    const suffix = util.randomString()
    const schema = [
      {
        '@id': `DObject${suffix}`,
        '@type': 'Class',
        '@abstract': [],
      },
      {
        '@id': `DAgent${suffix}`,
        '@type': 'Class',
        '@abstract': [],
        '@inherits': `DObject${suffix}`,
      },
      {
        '@id': `DXRobot${suffix}`,
        '@type': 'Class',
        '@inherits': `DAgent${suffix}`,
      },
      {
        '@id': `DEvent${suffix}`,
        '@type': 'Class',
        hasParticipant: { '@type': 'Set', '@class': `DObject${suffix}` },
      },
      {
        '@id': `DAction${suffix}`,
        '@type': 'Class',
        '@inherits': `DEvent${suffix}`,
        hasParticipant: `DAgent${suffix}`,
      },
    ]
    await document.insert(agent, { schema })
    const path = `/api/schema/admin/${agent.dbName}?type=DAction${suffix}`
    const r = await agent.get(path)
    expect(r.body.hasParticipant).to.deep.equal([`DXRobot${suffix}`])
    const all = await agent.get(`/api/schema/admin/${agent.dbName}`)
    expect(all.body[`DAction${suffix}`].hasParticipant).to.deep.equal([`DXRobot${suffix}`])
  })

  it('passes frame for a diamond narrowed to a cardinality range', async function () {
    // A subclass narrows an inherited Set of an abstract class to an
    // explicit Cardinality range of a more specific abstract class.
    // A bounded range is a subset of Set, so the narrower declaration
    // wins and the frame expands the abstract range.
    const suffix = util.randomString()
    const schema = [
      {
        '@id': `DObject${suffix}`,
        '@type': 'Class',
        '@abstract': [],
      },
      {
        '@id': `DAgent${suffix}`,
        '@type': 'Class',
        '@abstract': [],
        '@inherits': `DObject${suffix}`,
      },
      {
        '@id': `DXRobot${suffix}`,
        '@type': 'Class',
        '@inherits': `DAgent${suffix}`,
      },
      {
        '@id': `DEvent${suffix}`,
        '@type': 'Class',
        hasParticipant: { '@type': 'Set', '@class': `DObject${suffix}` },
      },
      {
        '@id': `DAction${suffix}`,
        '@type': 'Class',
        '@inherits': `DEvent${suffix}`,
        hasParticipant: {
          '@type': 'Cardinality',
          '@class': `DAgent${suffix}`,
          '@min_cardinality': 0,
          '@max_cardinality': 2,
        },
      },
    ]
    await document.insert(agent, { schema })
    const path = `/api/schema/admin/${agent.dbName}?type=DAction${suffix}`
    const r = await agent.get(path)
    expect(r.body.hasParticipant['@class']).to.deep.equal([`DXRobot${suffix}`])
    expect(r.body.hasParticipant['@min']).to.equal(0)
    expect(r.body.hasParticipant['@max']).to.equal(2)
  })

  it('passes JSON Schema for a class with format=jsonschema', async function () {
    const enumSchema = {
      '@id': util.randomString(),
      '@type': 'Enum',
      '@value': ['red', 'green'],
    }
    const schema = {
      '@id': util.randomString(),
      '@type': 'Class',
      '@key': { '@type': 'Lexical', '@fields': ['name'] },
      name: 'xsd:string',
      nickname: { '@type': 'Optional', '@class': 'xsd:string' },
      tags: { '@type': 'Set', '@class': 'xsd:string' },
      color: enumSchema['@id'],
    }
    await document.insert(agent, { schema: [enumSchema, schema] })
    const path = `/api/schema/admin/${agent.dbName}?type=${schema['@id']}&format=jsonschema`
    const r = await agent.get(path)
    const body = r.body
    expect(body.$schema).to.equal('https://json-schema.org/draft-07/schema#')
    expect(body.type).to.equal('object')
    expect(body.additionalProperties).to.equal(false)
    // @type and @id are supported optional keys with arbitrary values
    expect(body.properties['@type']).to.deep.equal({ type: 'string' })
    expect(body.properties['@id']).to.deep.equal({ type: 'string' })
    expect(body.properties.name).to.deep.equal({ type: 'string' })
    expect(body.properties.nickname).to.deep.equal({ type: 'string' })
    expect(body.properties.tags).to.deep.equal({
      type: 'array',
      uniqueItems: true,
      items: { type: 'string' },
    })
    // enum properties are $refs into definitions.EnumtraitValues (zebra)
    expect(body.properties.color).to.deep.equal({
      $ref: `#/definitions/EnumtraitValues/${enumSchema['@id']}`,
    })
    const enumDef = body.definitions.EnumtraitValues[enumSchema['@id']]
    expect(enumDef.type).to.equal('string')
    expect(enumDef.oneOf).to.have.length(2)
    expect(enumDef.oneOf[0]).to.have.property('enum')
    expect(body.required).to.have.members(['name', 'color', 'tags'])
    expect(body['x-terminusdb'].type_choices).to.deep.equal([schema['@id']])
    expect(body['x-terminusdb'].id_pattern).to.have.string(`^${schema['@id']}/`)
    expect(body['x-terminusdb'].key).to.deep.equal({
      '@type': 'Lexical',
      '@fields': ['name'],
    })
  })

  it('passes JSON Schema for all classes with format=jsonschema', async function () {
    const path = `/api/schema/admin/${agent.dbName}?format=jsonschema`
    const r = await agent.get(path)
    expect(r.body.$schema).to.equal('https://json-schema.org/draft-07/schema#')
    expect(r.body.definitions).to.be.an('object')
    expect(Object.keys(r.body.definitions)).to.not.be.empty
    // Class names are random, so the first definition is not necessarily
    // a class — enum definitions are keyed by the enum id.
    const defs = Object.values(r.body.definitions)
    expect(defs.some((d) => d.type === 'object')).to.equal(true)
  })
})
