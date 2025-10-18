const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('numeric-typecast-cross-family', function () {
  let agent

  before(async function () {
    this.timeout(200000)
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('IEEE 754 to Decimal Family Typecasts', function () {
    it('xsd:double → xsd:decimal should work', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '33.0',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:decimal',
        },
        result: {
          '@type': 'Value',
          variable: 'decimal',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].decimal['@type']).to.equal('xsd:decimal')
      expect(r.body.bindings[0].decimal['@value']).to.equal(33)
    })

    it('xsd:float → xsd:decimal should work', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:float',
            '@value': '33.0',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:decimal',
        },
        result: {
          '@type': 'Value',
          variable: 'decimal',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].decimal['@type']).to.equal('xsd:decimal')
      expect(r.body.bindings[0].decimal['@value']).to.equal(33)
    })

    it('xsd:double → xsd:integer should work (whole numbers)', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '33.0',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:integer',
        },
        result: {
          '@type': 'Value',
          variable: 'integer',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].integer['@type']).to.equal('xsd:integer')
      expect(r.body.bindings[0].integer['@value']).to.equal(33)
    })

    it('xsd:float → xsd:integer should work (whole numbers)', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:float',
            '@value': '42.0',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:integer',
        },
        result: {
          '@type': 'Value',
          variable: 'integer',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].integer['@type']).to.equal('xsd:integer')
      expect(r.body.bindings[0].integer['@value']).to.equal(42)
    })
  })

  describe('Float and Double Typecasts Between Each Other', function () {
    it('xsd:double → xsd:float should work', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '33.5',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:float',
        },
        result: {
          '@type': 'Value',
          variable: 'float',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].float['@type']).to.equal('xsd:float')
      expect(r.body.bindings[0].float['@value']).to.be.closeTo(33.5, 0.001)
    })

    it('xsd:float → xsd:double should work', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:float',
            '@value': '33.5',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:double',
        },
        result: {
          '@type': 'Value',
          variable: 'double',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].double['@type']).to.equal('xsd:double')
      expect(r.body.bindings[0].double['@value']).to.be.closeTo(33.5, 0.001)
    })
  })

  describe('Decimal Family to IEEE 754 Typecasts', function () {
    it('xsd:decimal → xsd:double should work', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:decimal',
            '@value': '33.5',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:double',
        },
        result: {
          '@type': 'Value',
          variable: 'double',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].double['@type']).to.equal('xsd:double')
      expect(r.body.bindings[0].double['@value']).to.be.closeTo(33.5, 0.001)
    })

    it('xsd:decimal → xsd:float should work', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:decimal',
            '@value': '33.5',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:float',
        },
        result: {
          '@type': 'Value',
          variable: 'float',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].float['@type']).to.equal('xsd:float')
      expect(r.body.bindings[0].float['@value']).to.be.closeTo(33.5, 0.001)
    })

    it('xsd:integer → xsd:double should work', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:integer',
            '@value': '33',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:double',
        },
        result: {
          '@type': 'Value',
          variable: 'double',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].double['@type']).to.equal('xsd:double')
      expect(r.body.bindings[0].double['@value']).to.equal(33)
    })

    it('xsd:integer → xsd:float should work', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:integer',
            '@value': '33',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:float',
        },
        result: {
          '@type': 'Value',
          variable: 'float',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].float['@type']).to.equal('xsd:float')
      expect(r.body.bindings[0].float['@value']).to.equal(33)
    })
  })

  describe('Edge Cases and Precision', function () {
    it('xsd:double(0.1) → xsd:decimal preserves IEEE 754 value', async function () {
      // When converting from IEEE 754 to decimal, we get the IEEE 754 value
      // NOT the "ideal" decimal 0.1
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '0.1',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:decimal',
        },
        result: {
          '@type': 'Value',
          variable: 'decimal',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      // IEEE 754 representation of 0.1 is ~0.1000000000000000055...
      const result = r.body.bindings[0].decimal['@value']
      expect(result).to.be.a('number')
      // It should be close to 0.1 but may have IEEE 754 artifacts
      expect(result).to.be.closeTo(0.1, 0.00000000000001)
    })

    it('xsd:decimal(0.1) → xsd:double loses exact precision', async function () {
      // When converting exact decimal to IEEE 754, precision may be lost
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:decimal',
            '@value': '0.1',
          },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:double',
        },
        result: {
          '@type': 'Value',
          variable: 'double',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      const result = r.body.bindings[0].double['@value']
      // Result will be IEEE 754 approximation of 0.1
      expect(result).to.be.closeTo(0.1, 0.00000000000001)
    })
  })
})
