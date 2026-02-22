const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('woql-comparison', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Less Than Operator', function () {
    it('passes with 5 < 10', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 5 },
        right: { '@type': 'DataValue', data: 10 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with 21.1 < 40.1', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 21.1 },
        right: { '@type': 'DataValue', data: 40.1 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with equal values (33 < 33)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 33 },
        right: { '@type': 'DataValue', data: 33 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('fails with equal decimal values (33.0 < 33.0)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 33.0 },
        right: { '@type': 'DataValue', data: 33.0 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('passes with cross-type: 21.1 < 40', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 21.1 },
        right: { '@type': 'DataValue', data: 40 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with cross-type: 21 < 40.1', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 21 },
        right: { '@type': 'DataValue', data: 40.1 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })
  })

  describe('Greater Than Operator', function () {
    it('passes with 10 > 5', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 10 },
        right: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with 40.1 > 21.1', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 40.1 },
        right: { '@type': 'DataValue', data: 21.1 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with equal values (33 > 33) - (issue #2225)', async function () {
      // This is the core regression test for Issue #2225
      // Before fix: 33 > 33 would incorrectly return true due to structural comparison
      // After fix: correctly returns false (empty bindings)
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 33 },
        right: { '@type': 'DataValue', data: 33 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('fails with equal decimal values (33.0 > 33.0)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 33.0 },
        right: { '@type': 'DataValue', data: 33.0 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('passes with cross-type: 40.1 > 21', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 40.1 },
        right: { '@type': 'DataValue', data: 21 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with cross-type: 40 > 21.1', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 40 },
        right: { '@type': 'DataValue', data: 21.1 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with cross-type equal values: 33.0 > 33', async function () {
      // Tests that float vs integer comparison works correctly for equal values
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 33.0 },
        right: { '@type': 'DataValue', data: 33 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })
  })

  describe('Greater Than or Equal (Gte)', function () {
    it('passes with 10 >= 5', async function () {
      const query = {
        '@type': 'Gte',
        left: { '@type': 'DataValue', data: 10 },
        right: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with equal values: 5 >= 5 (boundary)', async function () {
      const query = {
        '@type': 'Gte',
        left: { '@type': 'DataValue', data: 5 },
        right: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with 4 >= 5', async function () {
      const query = {
        '@type': 'Gte',
        left: { '@type': 'DataValue', data: 4 },
        right: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('passes with equal decimal: 21.1 >= 21.1', async function () {
      const query = {
        '@type': 'Gte',
        left: { '@type': 'DataValue', data: 21.1 },
        right: { '@type': 'DataValue', data: 21.1 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with date: 2024-03-01 >= 2024-02-29 (day after leap day)', async function () {
      const query = {
        '@type': 'Gte',
        left: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-03-01' },
        },
        right: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-02-29' },
        },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with equal date: leap day 2024-02-29 >= 2024-02-29', async function () {
      const query = {
        '@type': 'Gte',
        left: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-02-29' },
        },
        right: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-02-29' },
        },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })
  })

  describe('Less Than or Equal (Lte)', function () {
    it('passes with 3 <= 5', async function () {
      const query = {
        '@type': 'Lte',
        left: { '@type': 'DataValue', data: 3 },
        right: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with equal values: 5 <= 5 (boundary)', async function () {
      const query = {
        '@type': 'Lte',
        left: { '@type': 'DataValue', data: 5 },
        right: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with 6 <= 5', async function () {
      const query = {
        '@type': 'Lte',
        left: { '@type': 'DataValue', data: 6 },
        right: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('passes with date: 2024-02-28 <= 2024-02-29 (day before leap day)', async function () {
      const query = {
        '@type': 'Lte',
        left: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-02-28' },
        },
        right: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-02-29' },
        },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes with equal date: 2024-02-29 <= 2024-02-29', async function () {
      const query = {
        '@type': 'Lte',
        left: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-02-29' },
        },
        right: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-02-29' },
        },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with date: 2024-03-01 <= 2024-02-29', async function () {
      const query = {
        '@type': 'Lte',
        left: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-03-01' },
        },
        right: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-02-29' },
        },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })
  })

  describe('InRange as matcher (half-open [Start, End))', function () {
    it('passes with value within range: 5 in [1, 10)', async function () {
      const query = {
        '@type': 'InRange',
        value: { '@type': 'DataValue', data: 5 },
        start: { '@type': 'DataValue', data: 1 },
        end: { '@type': 'DataValue', data: 10 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('passes at start boundary (inclusive): 1 in [1, 10)', async function () {
      const query = {
        '@type': 'InRange',
        value: { '@type': 'DataValue', data: 1 },
        start: { '@type': 'DataValue', data: 1 },
        end: { '@type': 'DataValue', data: 10 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails at end boundary (exclusive): 10 in [1, 10)', async function () {
      const query = {
        '@type': 'InRange',
        value: { '@type': 'DataValue', data: 10 },
        start: { '@type': 'DataValue', data: 1 },
        end: { '@type': 'DataValue', data: 10 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('fails with value below range: 0 in [1, 10)', async function () {
      const query = {
        '@type': 'InRange',
        value: { '@type': 'DataValue', data: 0 },
        start: { '@type': 'DataValue', data: 1 },
        end: { '@type': 'DataValue', data: 10 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('fails with value above range: 11 in [1, 10)', async function () {
      const query = {
        '@type': 'InRange',
        value: { '@type': 'DataValue', data: 11 },
        start: { '@type': 'DataValue', data: 1 },
        end: { '@type': 'DataValue', data: 10 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('passes with date in range: 2024-06-15 in [2024-01-01, 2025-01-01)', async function () {
      const query = {
        '@type': 'InRange',
        value: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-06-15' },
        },
        start: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-01-01' },
        },
        end: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2025-01-01' },
        },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails with date at end (exclusive): 2025-01-01 in [2024-01-01, 2025-01-01)', async function () {
      const query = {
        '@type': 'InRange',
        value: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2025-01-01' },
        },
        start: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2024-01-01' },
        },
        end: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:date', '@value': '2025-01-01' },
        },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('passes with decimal in range: 5.5 in [1.0, 10.0)', async function () {
      const query = {
        '@type': 'InRange',
        value: { '@type': 'DataValue', data: 5.5 },
        start: { '@type': 'DataValue', data: 1.0 },
        end: { '@type': 'DataValue', data: 10.0 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })
  })

  describe('InRange as filter on generated values', function () {
    it('filters sequence output: generate 1..10, keep only [3, 7)', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Sequence',
            value: { '@type': 'DataValue', variable: 'i' },
            start: { '@type': 'DataValue', data: 1 },
            end: { '@type': 'DataValue', data: 10 },
          },
          {
            '@type': 'InRange',
            value: { '@type': 'DataValue', variable: 'i' },
            start: { '@type': 'DataValue', data: 3 },
            end: { '@type': 'DataValue', data: 7 },
          },
        ],
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(4)
      const values = r.body.bindings.map((b) => b.i['@value'])
      expect(values).to.deep.equal([3, 4, 5, 6])
    })

    it('filters sequence with step: generate 0,5,10,...,45, keep only [20, 35)', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Sequence',
            value: { '@type': 'DataValue', variable: 'n' },
            start: { '@type': 'DataValue', data: 0 },
            end: { '@type': 'DataValue', data: 50 },
            step: { '@type': 'DataValue', data: 5 },
          },
          {
            '@type': 'InRange',
            value: { '@type': 'DataValue', variable: 'n' },
            start: { '@type': 'DataValue', data: 20 },
            end: { '@type': 'DataValue', data: 35 },
          },
        ],
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(3)
      const values = r.body.bindings.map((b) => b.n['@value'])
      expect(values).to.deep.equal([20, 25, 30])
    })
  })

  describe('Sequence as generator', function () {
    it('generates correct values 1,2,3,4,5 for sequence(v:i, 1, 6)', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', variable: 'i' },
        start: { '@type': 'DataValue', data: 1 },
        end: { '@type': 'DataValue', data: 6 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(5)
      const values = r.body.bindings.map((b) => b.i['@value'])
      expect(values).to.deep.equal([1, 2, 3, 4, 5])
    })

    it('generates correct values 0,2,4,6,8 with step=2', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', variable: 'i' },
        start: { '@type': 'DataValue', data: 0 },
        end: { '@type': 'DataValue', data: 10 },
        step: { '@type': 'DataValue', data: 2 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(5)
      const values = r.body.bindings.map((b) => b.i['@value'])
      expect(values).to.deep.equal([0, 2, 4, 6, 8])
    })

    it('generates 0 values for empty range where start equals end', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', variable: 'i' },
        start: { '@type': 'DataValue', data: 5 },
        end: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('generates exactly one value for single-element range [7, 8)', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', variable: 'i' },
        start: { '@type': 'DataValue', data: 7 },
        end: { '@type': 'DataValue', data: 8 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].i['@value']).to.equal(7)
    })

    it('generates correct values 0,3,6 with step=3 for [0, 9)', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', variable: 'i' },
        start: { '@type': 'DataValue', data: 0 },
        end: { '@type': 'DataValue', data: 9 },
        step: { '@type': 'DataValue', data: 3 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(3)
      const values = r.body.bindings.map((b) => b.i['@value'])
      expect(values).to.deep.equal([0, 3, 6])
    })

    it('generates values usable in arithmetic: doubles each via Eval', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Sequence',
            value: { '@type': 'DataValue', variable: 'i' },
            start: { '@type': 'DataValue', data: 1 },
            end: { '@type': 'DataValue', data: 4 },
          },
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Times',
              left: { '@type': 'ArithmeticValue', variable: 'i' },
              right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 2 } },
            },
            result: { '@type': 'ArithmeticValue', variable: 'doubled' },
          },
        ],
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(3)
      const values = r.body.bindings.map((b) => b.doubled['@value'])
      expect(values).to.deep.equal([2, 4, 6])
    })
  })

  describe('Sequence as matcher (ground value)', function () {
    it('succeeds when ground value is in the sequence: 3 in [1, 6)', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', data: 3 },
        start: { '@type': 'DataValue', data: 1 },
        end: { '@type': 'DataValue', data: 6 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails when ground value is outside the sequence: 10 in [1, 6)', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', data: 10 },
        start: { '@type': 'DataValue', data: 1 },
        end: { '@type': 'DataValue', data: 6 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('succeeds when ground value is at start boundary: 0 in [0, 5)', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', data: 0 },
        start: { '@type': 'DataValue', data: 0 },
        end: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails when ground value is at end boundary (exclusive): 5 in [0, 5)', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', data: 5 },
        start: { '@type': 'DataValue', data: 0 },
        end: { '@type': 'DataValue', data: 5 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })

    it('succeeds when ground value matches a step: 6 in [0, 10) step 3', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', data: 6 },
        start: { '@type': 'DataValue', data: 0 },
        end: { '@type': 'DataValue', data: 10 },
        step: { '@type': 'DataValue', data: 3 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    })

    it('fails when ground value is between steps: 4 in [0, 10) step 3', async function () {
      const query = {
        '@type': 'Sequence',
        value: { '@type': 'DataValue', data: 4 },
        start: { '@type': 'DataValue', data: 0 },
        end: { '@type': 'DataValue', data: 10 },
        step: { '@type': 'DataValue', data: 3 },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })
  })
})
