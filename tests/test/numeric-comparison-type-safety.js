const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('numeric-comparison-type-safety', function () {
  let agent

  before(async function () {
    this.timeout(200000)
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('VALID: Rational Family Comparisons (xsd:integer ↔ xsd:decimal)', function () {
    it('xsd:integer == xsd:decimal should work', async function () {
      const query = {
        '@type': 'Equals',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:integer',
            '@value': '33',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:decimal',
            '@value': '33.0',
          },
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('xsd:integer < xsd:decimal should work', async function () {
      const query = {
        '@type': 'Less',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:integer',
            '@value': '10',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:decimal',
            '@value': '20.5',
          },
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('xsd:decimal > xsd:integer should work', async function () {
      const query = {
        '@type': 'Greater',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:decimal',
            '@value': '50.5',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:integer',
            '@value': '25',
          },
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('VALID: IEEE 754 Family Comparisons (xsd:float ↔ xsd:double)', function () {
    it('xsd:float == xsd:double should work', async function () {
      const query = {
        '@type': 'Equals',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:float',
            '@value': '33.0',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '33.0',
          },
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('xsd:float < xsd:double should work', async function () {
      const query = {
        '@type': 'Less',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:float',
            '@value': '10.5',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '20.5',
          },
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('xsd:double > xsd:float should work', async function () {
      const query = {
        '@type': 'Greater',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '100.5',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:float',
            '@value': '50.5',
          },
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('INVALID: Cross-Family Comparisons Should Fail', function () {
    it('xsd:double == xsd:decimal should fail with type error', async function () {
      const query = {
        '@type': 'Equals',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '33.0',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:decimal',
            '@value': '33.0',
          },
        },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.status).to.equal(400)
      expect(r.body['api:message']).to.match(/incompatible|cast/i)
    })

    it('xsd:float == xsd:integer should fail with type error', async function () {
      const query = {
        '@type': 'Equals',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:float',
            '@value': '42.0',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:integer',
            '@value': '42',
          },
        },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.status).to.equal(400)
      expect(r.body['api:message']).to.match(/incompatible|cast/i)
    })

    it('xsd:double < xsd:decimal should fail with type error', async function () {
      const query = {
        '@type': 'Less',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '10.0',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:decimal',
            '@value': '20.0',
          },
        },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.status).to.equal(400)
      expect(r.body['api:message']).to.match(/incompatible|cast/i)
    })

    it('xsd:float > xsd:integer should fail with type error', async function () {
      const query = {
        '@type': 'Greater',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:float',
            '@value': '100.0',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:integer',
            '@value': '50',
          },
        },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.status).to.equal(400)
      expect(r.body['api:message']).to.match(/incompatible|cast/i)
    })

    it('xsd:decimal < xsd:float should fail with type error', async function () {
      const query = {
        '@type': 'Less',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:decimal',
            '@value': '5.5',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:float',
            '@value': '10.5',
          },
        },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.status).to.equal(400)
      expect(r.body['api:message']).to.match(/incompatible|cast/i)
    })

    it('xsd:integer > xsd:double should fail with type error', async function () {
      const query = {
        '@type': 'Greater',
        left: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:integer',
            '@value': '100',
          },
        },
        right: {
          '@type': 'Value',
          data: {
            '@type': 'xsd:double',
            '@value': '50.0',
          },
        },
      }

      const r = await woql.post(agent, query).fails()
      expect(r.status).to.equal(400)
      expect(r.body['api:message']).to.match(/incompatible|cast/i)
    })
  })

  // For full documentation of type family rules, see:
  // docs/NUMERIC_TYPE_SAFETY.md
})
