const { expect } = require('chai')
const Ajv = require('ajv')
const { Agent, db, document, util } = require('../lib')

// Dual-verification suite: for each schema construct we download the
// generated JSON Schema from the endpoint, validate candidate
// documents client-side with ajv, then write the same documents to
// TerminusDB and assert the same accept/reject verdict. Gaps between
// the two verdicts surface fidelity bugs in the converter.
describe('jsonschema', function () {
  let agent
  const ajv = new Ajv({ allErrors: true })

  async function jsonSchema (type) {
    const query = type ? `?type=${type}&format=jsonschema` : '?format=jsonschema'
    const r = await agent.get(`/api/schema/admin/${agent.dbName}${query}`)
    return r.body
  }

  function validator (schema) {
    // $schema is informational; ajv6 resolves the draft-07 meta via
    // the http:// URI while the endpoint advertises https://.
    delete schema.$schema
    const validate = ajv.compile(schema)
    return {
      valid (doc) {
        const ok = validate(doc)
        expect(ok, JSON.stringify(validate.errors)).to.equal(true)
      },
      invalid (doc) {
        expect(validate(doc)).to.equal(false)
      },
    }
  }

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('cardinality and collection wrappers', function () {
    const person = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'Lexical', '@fields': ['name'] },
      '@documentation': {
        '@label': 'Person',
        '@comment': 'A person',
        '@properties': { name: { '@label': 'Full name' } },
      },
      name: 'xsd:string',
      nickname: { '@type': 'Optional', '@class': 'xsd:string' },
      tags: { '@type': 'Set', '@class': 'xsd:string' },
      history: { '@type': 'List', '@class': 'xsd:string' },
      scores: { '@type': 'Cardinality', '@class': 'xsd:integer', '@min_cardinality': 1, '@max_cardinality': 2 },
    }

    before(async function () {
      person['@id'] = util.randomString()
      await document.insert(agent, { schema: person })
      this.v = validator(await jsonSchema(person['@id']))
    })

    it('accepts a fully populated document in both validators', async function () {
      const doc = {
        '@type': person['@id'],
        name: 'Ann',
        nickname: 'Annie',
        tags: ['a', 'b'],
        history: ['first', 'second'],
        scores: [1, 2],
      }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('accepts a minimal document in both validators', async function () {
      const doc = { '@type': person['@id'], name: 'Bob', tags: [], history: [], scores: [7] }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('rejects a document missing a required property', async function () {
      const doc = { '@type': person['@id'], '@id': `${person['@id']}/no-name`, scores: [1] }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })

    it('rejects a document violating cardinality bounds', async function () {
      const doc = { '@type': person['@id'], '@id': `${person['@id']}/too-many`, name: 'Cal', scores: [1, 2, 3] }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })

    it('rejects a document with the wrong scalar type', async function () {
      const doc = { '@type': person['@id'], '@id': `${person['@id']}/bad-type`, name: 'Dan', scores: [true] }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })

    it('rejects duplicate set members', async function () {
      const doc = { '@type': person['@id'], '@id': `${person['@id']}/dup`, name: 'Eve', scores: [1], tags: ['x', 'x'] }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })
  })

  describe('enum membership', function () {
    const color = {
      '@id': null,
      '@type': 'Enum',
      '@value': ['red', 'green', 'blue'],
    }
    const thing = {
      '@id': null,
      '@type': 'Class',
      color,
      count: 'xsd:nonNegativeInteger',
    }

    before(async function () {
      color['@id'] = util.randomString()
      thing['@id'] = util.randomString()
      await document.insert(agent, { schema: [color, thing] })
      this.v = validator(await jsonSchema(thing['@id']))
    })

    it('accepts a valid enum member in both validators', async function () {
      const doc = { '@type': thing['@id'], color: 'green', count: 4 }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('rejects a non-member enum value', async function () {
      const doc = { '@type': thing['@id'], '@id': `${thing['@id']}/bad`, color: 'purple', count: 0 }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })

    it('rejects a negative nonNegativeInteger', async function () {
      const doc = { '@type': thing['@id'], '@id': `${thing['@id']}/neg`, color: 'red', count: -1 }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })
  })

  describe('subdocuments and recursion', function () {
    const line = {
      '@id': null,
      '@type': 'Class',
      '@subdocument': [],
      '@key': { '@type': 'ValueHash' },
      start: 'xsd:decimal',
      next: { '@type': 'Optional', '@class': null }, // patched below
    }
    const drawing = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      strokes: { '@type': 'List', '@class': null }, // patched below
      frame: null, // patched below
    }

    before(async function () {
      line['@id'] = util.randomString()
      drawing['@id'] = util.randomString()
      line.next['@class'] = line['@id']
      drawing.strokes['@class'] = line['@id']
      drawing.frame = line['@id']
      await document.insert(agent, { schema: [line, drawing] })
      this.v = validator(await jsonSchema(drawing['@id']))
    })

    it('accepts a document with nested subdocuments', async function () {
      const doc = {
        '@type': drawing['@id'],
        strokes: [
          { '@type': line['@id'], start: 1.5 },
          { '@type': line['@id'], start: 2.5, next: { '@type': line['@id'], start: 3.5 } },
        ],
        frame: { '@type': line['@id'], start: 0.5 },
      }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('rejects a subdocument with a missing required field', async function () {
      const doc = {
        '@type': drawing['@id'],
        '@id': `${drawing['@id']}/bad`,
        frame: { '@type': line['@id'] },
      }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })

    it('validates deep recursion through generated definitions', async function () {
      // deeper than the inline depth: validation must follow $ref
      const deep = {
        '@type': line['@id'],
        start: 0.5,
        next: {
          '@type': line['@id'],
          start: 1.5,
          next: {
            '@type': line['@id'],
            start: 2.5,
            next: { '@type': line['@id'], start: 3.5, next: { '@type': line['@id'], start: 4.5 } },
          },
        },
      }
      const lineV = validator(await jsonSchema(line['@id']))
      lineV.valid(deep)
      const wrapper = { '@type': drawing['@id'], strokes: [deep], frame: { '@type': line['@id'], start: 9.5 } }
      this.v.valid(wrapper)
      await document.insert(agent, { instance: wrapper })
    })
  })

  describe('inheritance and tagged unions', function () {
    const shape = {
      '@id': null,
      '@type': 'Class',
      '@abstract': [],
      label: 'xsd:string',
    }
    const circle = {
      '@id': null,
      '@type': 'Class',
      '@inherits': [], // patched below: inherits abstract class
      radius: 'xsd:decimal',
    }
    const board = {
      '@id': null,
      '@type': 'Class',
      figure: null, // patched below: link to abstract class
      measurement: {
        '@id': null,
        '@type': 'TaggedUnion',
        '@subdocument': [],
        '@key': { '@type': 'ValueHash' },
        area: 'xsd:decimal',
        perimeter: 'xsd:decimal',
      },
    }

    before(async function () {
      shape['@id'] = util.randomString()
      circle['@id'] = util.randomString()
      board['@id'] = util.randomString()
      board.measurement['@id'] = util.randomString()
      board.figure = shape['@id']
      circle['@inherits'] = [shape['@id']]
      await document.insert(agent, { schema: [shape, circle, board] })
      this.boardV = validator(await jsonSchema(board['@id']))
      this.circleV = validator(await jsonSchema(circle['@id']))
    })

    it('accepts an instance of the concrete subclass', async function () {
      const doc = { '@type': circle['@id'], label: 'wheel', radius: 3.25 }
      this.circleV.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('rejects an instance missing an inherited property', async function () {
      const doc = { '@type': circle['@id'], '@id': `${circle['@id']}/unlabelled`, radius: 1.5 }
      this.circleV.invalid(doc)
      await expectInsertFailure(doc)
    })

    it('accepts a document linking to a subclass of an abstract class', async function () {
      const figure = { '@type': circle['@id'], '@id': `${circle['@id']}/linked`, label: 'dial', radius: 0.5 }
      const doc = {
        '@type': board['@id'],
        figure: figure['@id'],
        measurement: { '@type': board.measurement['@id'], area: 9.75 },
      }
      this.boardV.valid(doc)
      await document.insert(agent, { instance: [figure, doc] })
    })

    it('rejects a tagged union document carrying both alternatives', async function () {
      const doc = {
        '@type': board['@id'],
        '@id': `${board['@id']}/bad-union`,
        figure: `${circle['@id']}/linked`,
        measurement: { '@type': board.measurement['@id'], area: 1.5, perimeter: 2.5 },
      }
      // JSON Schema oneOf enforces exactly one alternative.
      this.boardV.invalid(doc)
    })
  })

  describe('foreign types', function () {
    const passport = { '@id': null, '@type': 'Foreign' }
    const citizen = {
      '@id': null,
      '@type': 'Class',
      name: 'xsd:string',
      papers: null, // patched below
    }

    before(async function () {
      passport['@id'] = util.randomString()
      citizen['@id'] = util.randomString()
      citizen.papers = passport['@id']
      await document.insert(agent, { schema: [passport, citizen] })
      this.v = validator(await jsonSchema(citizen['@id']))
    })

    it('accepts a document with a foreign link', async function () {
      const doc = { '@type': citizen['@id'], name: 'Fia', papers: `${passport['@id']}/X123` }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
    })
  })

  describe('key strategies', function () {
    const hashed = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'Hash', '@fields': ['name'] },
      name: 'xsd:string',
    }
    const valued = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'ValueHash' },
      payload: 'xsd:string',
    }
    const random = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      label: 'xsd:string',
    }
    const uuid = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'UuidV7' },
      label: 'xsd:string',
    }

    before(async function () {
      for (const cls of [hashed, valued, random, uuid]) {
        cls['@id'] = util.randomString()
      }
      await document.insert(agent, { schema: [hashed, valued, random, uuid] })
    })

    it('accepts documents for every key strategy', async function () {
      for (const [cls, field] of [[hashed, 'name'], [valued, 'payload'], [random, 'label'], [uuid, 'label']]) {
        const doc = { '@type': cls['@id'], [field]: 'value' }
        const v = validator(await jsonSchema(cls['@id']))
        v.valid(doc)
        const r = await document.insert(agent, { instance: doc })
        expect(r.body).to.be.an('array').with.lengthOf(1)
      }
    })

    it('records the key strategy in x-terminusdb', async function () {
      for (const [cls, expected] of [[hashed, 'Hash'], [valued, 'ValueHash'], [random, 'Random'], [uuid, 'UuidV7']]) {
        const schema = await jsonSchema(cls['@id'])
        expect(schema['x-terminusdb'].key['@type']).to.equal(expected)
      }
    })
  })

  describe('xsd scalar coverage', function () {
    const sink = { '@id': null, '@type': 'Class', '@key': { '@type': 'Random' } }
    const fields = {
      str: 'xsd:string',
      normalized: 'xsd:normalizedString',
      tok: 'xsd:token',
      lang: 'xsd:language',
      flag: 'xsd:boolean',
      int: 'xsd:integer',
      small: 'xsd:short',
      byte: 'xsd:unsignedByte',
      dec: 'xsd:decimal',
      dbl: 'xsd:double',
      flt: 'xsd:float',
      date: 'xsd:date',
      dt: 'xsd:dateTime',
      tm: 'xsd:time',
      uri: 'xsd:anyURI',
      b64: 'xsd:base64Binary',
      year: 'xsd:gYear',
      dur: { '@type': 'Optional', '@class': 'xsd:duration' },
      unit: { '@type': 'Optional', '@class': 'sys:Unit' },
      blob: { '@type': 'Optional', '@class': 'sys:JSON' },
    }

    before(async function () {
      sink['@id'] = util.randomString()
      Object.assign(sink, fields)
      await document.insert(agent, { schema: sink })
      this.v = validator(await jsonSchema(sink['@id']))
    })

    it('accepts a document spanning the scalar range', async function () {
      const doc = {
        '@type': sink['@id'],
        str: 'text',
        normalized: 'text',
        tok: 'tok',
        lang: 'en',
        flag: true,
        int: -42,
        small: 7,
        byte: 255,
        dec: 1.5,
        dbl: 2.5,
        flt: 3.5,
        date: '2024-01-31',
        dt: '2024-01-31T12:00:00Z',
        tm: '12:00:00',
        uri: 'https://example.com/x',
        b64: 'aGk=',
        year: '1999',
        dur: 'P1Y',
        blob: { arbitrary: ['json', 1, null] },
      }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('rejects a string where an integer is required', async function () {
      const doc = { '@type': sink['@id'], '@id': `${sink['@id']}/bad`, int: 'not a number' }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })
  })

  describe('set cardinality constraints', function () {
    const team = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      trio: { '@type': 'Set', '@class': 'xsd:string', '@cardinality': 3 },
      bounded: { '@type': 'Set', '@class': 'xsd:integer', '@min_cardinality': 1, '@max_cardinality': 2 },
    }

    before(async function () {
      team['@id'] = util.randomString()
      await document.insert(agent, { schema: team })
      this.v = validator(await jsonSchema(team['@id']))
    })

    it('accepts documents at the exact and bounded cardinalities', async function () {
      const doc = {
        '@type': team['@id'],
        trio: ['a', 'b', 'c'],
        bounded: [1, 2],
      }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('rejects an off-count exact cardinality', async function () {
      const doc = { '@type': team['@id'], '@id': `${team['@id']}/bad`, trio: ['a', 'b'], bounded: [1] }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })

    it('rejects over-max bounded cardinality', async function () {
      const doc = { '@type': team['@id'], '@id': `${team['@id']}/over`, trio: ['a', 'b', 'c'], bounded: [1, 2, 3] }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })
  })

  describe('multi-dimensional array', function () {
    const polygon = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      coordinates: { '@type': 'Array', '@dimensions': 2, '@class': 'xsd:decimal' },
    }

    before(async function () {
      polygon['@id'] = util.randomString()
      await document.insert(agent, { schema: polygon })
      this.v = validator(await jsonSchema(polygon['@id']))
    })

    it('accepts a nested array document', async function () {
      const doc = {
        '@type': polygon['@id'],
        coordinates: [[-6.34, 53.37], [-6.33, 53.37], [-6.34, 53.36]],
      }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('rejects a flat array where two dimensions are required', async function () {
      const doc = { '@type': polygon['@id'], '@id': `${polygon['@id']}/flat`, coordinates: [1.5, 2.5] }
      this.v.invalid(doc)
    })
  })

  describe('class-level @oneOf', function () {
    const intOrString = {
      '@id': null,
      '@type': 'Class',
      '@oneOf': { integer: 'xsd:integer', string: 'xsd:string' },
    }
    const pet = {
      '@id': null,
      '@type': 'Class',
      name: 'xsd:string',
      '@oneOf': [
        { cat: 'xsd:string', dog: 'xsd:string' },
        { employers: 'xsd:positiveInteger', unemployed: 'xsd:string' },
      ],
    }

    before(async function () {
      intOrString['@id'] = util.randomString()
      pet['@id'] = util.randomString()
      await document.insert(agent, { schema: [intOrString, pet] })
      this.iv = validator(await jsonSchema(intOrString['@id']))
      this.pv = validator(await jsonSchema(pet['@id']))
    })

    it('accepts one alternative of a single group', async function () {
      const doc = { '@type': intOrString['@id'], integer: 0 }
      this.iv.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('rejects both alternatives of a single group', async function () {
      const doc = { '@type': intOrString['@id'], '@id': `${intOrString['@id']}/both`, integer: 0, string: 'zero' }
      this.iv.invalid(doc)
      await expectInsertFailure(doc)
    })

    it('accepts one member from each of two groups', async function () {
      const doc = { '@type': pet['@id'], name: 'Rex', cat: 'whiskers', employers: 5 }
      this.pv.valid(doc)
      await document.insert(agent, { instance: doc })
    })
  })

  describe('choice subdocuments', function () {
    // A property over an abstract class with concrete subdocument
    // subclasses accepts any alternative; the schema is an anyOf.
    const critter = { '@id': null, '@type': 'Class', '@abstract': [], '@subdocument': [], '@key': { '@type': 'ValueHash' }, petname: 'xsd:string' }
    const cat = { '@id': null, '@type': 'Class', '@inherits': [], lives: 'xsd:integer' }
    const dog = { '@id': null, '@type': 'Class', '@inherits': [], barks: 'xsd:boolean' }
    const house = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      pet: null, // patched below
    }

    before(async function () {
      for (const cls of [critter, cat, dog, house]) cls['@id'] = util.randomString()
      cat['@inherits'] = [critter['@id']]
      dog['@inherits'] = [critter['@id']]
      house.pet = critter['@id']
      await document.insert(agent, { schema: [critter, cat, dog, house] })
      this.v = validator(await jsonSchema(house['@id']))
    })

    it('accepts either alternative as the property value', async function () {
      const doc = { '@type': house['@id'], pet: { '@type': cat['@id'], petname: 'Tom', lives: 9 } }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
    })

    it('rejects an object matching no alternative', async function () {
      const doc = { '@type': house['@id'], '@id': `${house['@id']}/bad`, pet: { '@type': cat['@id'], petname: 'Tom' } }
      this.v.invalid(doc)
      await expectInsertFailure(doc)
    })
  })

  describe('unfoldable, unfold and shared', function () {
    const customer = { '@id': null, '@type': 'Class', name: 'xsd:string', email: 'xsd:string' }
    const product = {
      '@id': null,
      '@type': 'Class',
      '@shared': [],
      '@unfoldable': [],
      '@key': { '@type': 'Lexical', '@fields': ['name'] },
      name: 'xsd:string',
      price: 'xsd:decimal',
    }
    const order = {
      '@id': null,
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      orderNumber: 'xsd:string',
      customer: { '@type': 'Optional', '@class': null, '@unfold': true },
      items: { '@type': 'Set', '@class': null },
    }

    before(async function () {
      for (const cls of [customer, product, order]) cls['@id'] = util.randomString()
      order.customer['@class'] = customer['@id']
      order.items['@class'] = product['@id']
      await document.insert(agent, { schema: [customer, product, order] })
      this.v = validator(await jsonSchema(order['@id']))
    })

    it('accepts a document with shared and unfolded references', async function () {
      const prod = { '@type': product['@id'], '@id': `${product['@id']}/widget`, name: 'widget', price: 9.99 }
      const doc = {
        '@type': order['@id'],
        orderNumber: 'ORD-1',
        customer: `${customer['@id']}/alice`,
        items: [prod['@id']],
      }
      this.v.valid(doc)
      await document.insert(agent, { instance: [{ '@type': customer['@id'], '@id': `${customer['@id']}/alice`, name: 'Alice', email: 'a@b.c' }, prod, doc] })
    })

    it('carries the lifecycle flags into x-terminusdb', async function () {
      const schema = await jsonSchema(product['@id'])
      expect(schema['x-terminusdb'].shared).to.equal(true)
      expect(schema['x-terminusdb'].unfoldable).to.equal(true)
    })
  })

  describe('multiple inheritance', function () {
    const named = { '@id': null, '@type': 'Class', name: 'xsd:string' }
    const priced = { '@id': null, '@type': 'Class', price: 'xsd:decimal' }
    const catalogItem = {
      '@id': null,
      '@type': 'Class',
      '@inherits': [], // patched below
      sku: 'xsd:string',
    }

    before(async function () {
      for (const cls of [named, priced, catalogItem]) cls['@id'] = util.randomString()
      catalogItem['@inherits'] = [named['@id'], priced['@id']]
      await document.insert(agent, { schema: [named, priced, catalogItem] })
      this.v = validator(await jsonSchema(catalogItem['@id']))
    })

    it('requires properties from every parent', async function () {
      const doc = { '@type': catalogItem['@id'], name: 'thing', price: 1.0, sku: 'SKU-1' }
      this.v.valid(doc)
      await document.insert(agent, { instance: doc })
      const partial = { '@type': catalogItem['@id'], '@id': `${catalogItem['@id']}/partial`, name: 'x', sku: 's' }
      this.v.invalid(partial)
      await expectInsertFailure(partial)
    })
  })

  // Assert TerminusDB rejects the document on insert, without
  // pinning the exact error witnesses.
  async function expectInsertFailure (doc) {
    const r = await agent
      .post(`/api/document/admin/${agent.dbName}`)
      .query({ graph_type: 'instance', author: 'test', message: 'test' })
      .send(doc)
      .ok(() => true)
    expect(r.status, JSON.stringify(r.body)).to.equal(400)
    expect(r.body['api:status']).to.equal('api:failure')
  }

  // The check surface lives behind `check=true` on the document
  // endpoint — a dry run reports a verdict without writing.
  describe('check=true dry run', function () {
    let className

    before(async function () {
      className = util.randomString()
      await document.insert(agent, {
        schema: {
          '@id': className,
          '@type': 'Class',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        },
      })
    })

    it('reports an invalid document without committing it', async function () {
      const path = `/api/document/admin/${agent.dbName}?check=true&author=a&message=m`
      const r = await agent.post(path).send({ '@type': className })
      expect(r.body['api:valid']).to.equal(false)
      expect(r.body['api:witnesses']).to.not.be.empty
    })

    it('reports a valid document with generated ids', async function () {
      const path = `/api/document/admin/${agent.dbName}?check=true&author=a&message=m`
      const r = await agent.post(path).send({ '@type': className, name: 'Checked' })
      expect(r.body['api:valid']).to.equal(true)
      expect(r.body['api:extra'].ids).to.have.lengthOf(1)
      expect(r.body['api:extra'].ids[0]).to.contain(`${className}/`)
    })

    it('leaves the data untouched', async function () {
      const r = await agent.get(`/api/document/admin/${agent.dbName}?type=${className}&as_list=true`)
      expect(r.body).to.be.an('array').that.is.empty
    })

    it('reports a valid delete without committing it', async function () {
      const inserted = await document.insert(agent, {
        instance: { '@type': className, name: 'ToDelete' },
      })
      const id = inserted.body[0]
      const path = `/api/document/admin/${agent.dbName}?check=true&author=a&message=m&id=${id}`
      const r = await agent.delete(path)
      expect(r.body['api:valid']).to.equal(true)
      const found = await agent.get(
        `/api/document/admin/${agent.dbName}?id=${id}`)
      expect(found.status).to.equal(200)
    })
  })
})
