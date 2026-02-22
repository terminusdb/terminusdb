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

  describe('MonthStartDate', function () {
    it('computes first day of January 2024', async function () {
      const query = {
        '@type': 'MonthStartDate',
        year_month: { '@type': 'DataValue', data: { '@type': 'xsd:gYearMonth', '@value': '2024-01' } },
        date: { '@type': 'DataValue', variable: 'd' },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('2024-01-01')
    })

    it('computes first day of December 2023', async function () {
      const query = {
        '@type': 'MonthStartDate',
        year_month: { '@type': 'DataValue', data: { '@type': 'xsd:gYearMonth', '@value': '2023-12' } },
        date: { '@type': 'DataValue', variable: 'd' },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('2023-12-01')
    })
  })

  describe('MonthEndDate', function () {
    it('computes last day of January (31 days)', async function () {
      const query = {
        '@type': 'MonthEndDate',
        year_month: { '@type': 'DataValue', data: { '@type': 'xsd:gYearMonth', '@value': '2024-01' } },
        date: { '@type': 'DataValue', variable: 'd' },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('2024-01-31')
    })

    it('handles leap year February: 2024-02 -> 2024-02-29', async function () {
      const query = {
        '@type': 'MonthEndDate',
        year_month: { '@type': 'DataValue', data: { '@type': 'xsd:gYearMonth', '@value': '2024-02' } },
        date: { '@type': 'DataValue', variable: 'd' },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('2024-02-29')
    })

    it('handles non-leap year February: 2023-02 -> 2023-02-28', async function () {
      const query = {
        '@type': 'MonthEndDate',
        year_month: { '@type': 'DataValue', data: { '@type': 'xsd:gYearMonth', '@value': '2023-02' } },
        date: { '@type': 'DataValue', variable: 'd' },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('2023-02-28')
    })

    it('handles century leap year: 2000-02 -> 2000-02-29', async function () {
      const query = {
        '@type': 'MonthEndDate',
        year_month: { '@type': 'DataValue', data: { '@type': 'xsd:gYearMonth', '@value': '2000-02' } },
        date: { '@type': 'DataValue', variable: 'd' },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('2000-02-29')
    })

    it('handles century non-leap year: 1900-02 -> 1900-02-28', async function () {
      const query = {
        '@type': 'MonthEndDate',
        year_month: { '@type': 'DataValue', data: { '@type': 'xsd:gYearMonth', '@value': '1900-02' } },
        date: { '@type': 'DataValue', variable: 'd' },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('1900-02-28')
    })
  })

  describe('MonthStartDates (generator)', function () {
    it('generates 12 first-of-month dates in FY2024', async function () {
      const query = {
        '@type': 'MonthStartDates',
        date: { '@type': 'DataValue', variable: 'd' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:date', '@value': '2024-01-01' } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:date', '@value': '2025-01-01' } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(12)
      expect(r.body.bindings[0].d['@value']).to.equal('2024-01-01')
      expect(r.body.bindings[11].d['@value']).to.equal('2024-12-01')
    })

    it('generates 3 first-of-month dates in a quarter', async function () {
      const query = {
        '@type': 'MonthStartDates',
        date: { '@type': 'DataValue', variable: 'd' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:date', '@value': '2024-04-01' } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:date', '@value': '2024-07-01' } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(3)
      const values = r.body.bindings.map((b) => b.d['@value'])
      expect(values).to.deep.equal(['2024-04-01', '2024-05-01', '2024-06-01'])
    })
  })

  describe('MonthEndDates (generator)', function () {
    it('generates 12 last-of-month dates in FY2024', async function () {
      const query = {
        '@type': 'MonthEndDates',
        date: { '@type': 'DataValue', variable: 'd' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:date', '@value': '2024-01-01' } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:date', '@value': '2025-01-01' } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(12)
      expect(r.body.bindings[0].d['@value']).to.equal('2024-01-31')
      expect(r.body.bindings[1].d['@value']).to.equal('2024-02-29')
      expect(r.body.bindings[11].d['@value']).to.equal('2024-12-31')
    })

    it('generates correct Feb end dates across leap/non-leap boundary', async function () {
      const query = {
        '@type': 'MonthEndDates',
        date: { '@type': 'DataValue', variable: 'd' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:date', '@value': '2023-01-01' } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:date', '@value': '2025-01-01' } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(24)
      expect(r.body.bindings[1].d['@value']).to.equal('2023-02-28')
      expect(r.body.bindings[13].d['@value']).to.equal('2024-02-29')
    })
  })

  describe('IntervalRelation (Allen\'s Interval Algebra)', function () {
    function intervalQuery (relation, xs, xe, ys, ye) {
      const q = {
        '@type': 'IntervalRelation',
        x_start: { '@type': 'DataValue', data: xs },
        x_end: { '@type': 'DataValue', data: xe },
        y_start: { '@type': 'DataValue', data: ys },
        y_end: { '@type': 'DataValue', data: ye },
      }
      if (typeof relation === 'string') {
        q.relation = { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': relation } }
      } else {
        q.relation = relation
      }
      return q
    }
    function dec (v) { return { '@type': 'xsd:decimal', '@value': v } }
    function dat (v) { return { '@type': 'xsd:date', '@value': v } }

    // 7 fundamental relations
    it('validates before: [1,3) before [5,8)', async function () {
      const r = await woql.post(agent, intervalQuery('before', dec(1), dec(3), dec(5), dec(8)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('rejects before when intervals overlap: [1,5) not before [3,8)', async function () {
      const r = await woql.post(agent, intervalQuery('before', dec(1), dec(5), dec(3), dec(8)))
      expect(r.body.bindings).to.have.lengthOf(0)
    })
    it('validates meets: [1,5) meets [5,10)', async function () {
      const r = await woql.post(agent, intervalQuery('meets', dec(1), dec(5), dec(5), dec(10)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('rejects meets when gap exists: [1,4) does not meet [5,10)', async function () {
      const r = await woql.post(agent, intervalQuery('meets', dec(1), dec(4), dec(5), dec(10)))
      expect(r.body.bindings).to.have.lengthOf(0)
    })
    it('validates overlaps: [1,6) overlaps [4,10)', async function () {
      const r = await woql.post(agent, intervalQuery('overlaps', dec(1), dec(6), dec(4), dec(10)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validates starts: [1,5) starts [1,10)', async function () {
      const r = await woql.post(agent, intervalQuery('starts', dec(1), dec(5), dec(1), dec(10)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validates during: [3,7) during [1,10)', async function () {
      const r = await woql.post(agent, intervalQuery('during', dec(3), dec(7), dec(1), dec(10)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validates finishes: [5,10) finishes [1,10)', async function () {
      const r = await woql.post(agent, intervalQuery('finishes', dec(5), dec(10), dec(1), dec(10)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validates equals: [1,10) equals [1,10)', async function () {
      const r = await woql.post(agent, intervalQuery('equals', dec(1), dec(10), dec(1), dec(10)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('rejects equals when endpoints differ: [1,10) != [1,9)', async function () {
      const r = await woql.post(agent, intervalQuery('equals', dec(1), dec(10), dec(1), dec(9)))
      expect(r.body.bindings).to.have.lengthOf(0)
    })

    // 6 inverse relations
    it('validates after (inverse of before): [5,8) after [1,3)', async function () {
      const r = await woql.post(agent, intervalQuery('after', dec(5), dec(8), dec(1), dec(3)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validates met_by (inverse of meets): [5,10) met_by [1,5)', async function () {
      const r = await woql.post(agent, intervalQuery('met_by', dec(5), dec(10), dec(1), dec(5)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validates overlapped_by: [4,10) overlapped_by [1,6)', async function () {
      const r = await woql.post(agent, intervalQuery('overlapped_by', dec(4), dec(10), dec(1), dec(6)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validates started_by: [1,10) started_by [1,5)', async function () {
      const r = await woql.post(agent, intervalQuery('started_by', dec(1), dec(10), dec(1), dec(5)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validates contains (inverse of during): [1,10) contains [3,7)', async function () {
      const r = await woql.post(agent, intervalQuery('contains', dec(1), dec(10), dec(3), dec(7)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validates finished_by: [1,10) finished_by [5,10)', async function () {
      const r = await woql.post(agent, intervalQuery('finished_by', dec(1), dec(10), dec(5), dec(10)))
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    // Classification mode (relation as variable)
    it('classifies relation as "before" when [1,3) vs [5,8)', async function () {
      const q = intervalQuery(
        { '@type': 'NodeValue', variable: 'v:rel' },
        dec(1), dec(3), dec(5), dec(8))
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:rel']['@value']).to.equal('before')
    })
    it('classifies relation as "during" when [3,7) vs [1,10)', async function () {
      const q = intervalQuery(
        { '@type': 'NodeValue', variable: 'v:rel' },
        dec(3), dec(7), dec(1), dec(10))
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:rel']['@value']).to.equal('during')
    })
    it('classifies relation as "equals" when [1,10) vs [1,10)', async function () {
      const q = intervalQuery(
        { '@type': 'NodeValue', variable: 'v:rel' },
        dec(1), dec(10), dec(1), dec(10))
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:rel']['@value']).to.equal('equals')
    })

    // Date-based tests
    it('Q1 meets Q2 with dates (half-open adjacency)', async function () {
      const r = await woql.post(agent, intervalQuery('meets',
        dat('2024-01-01'), dat('2024-04-01'),
        dat('2024-04-01'), dat('2024-07-01')))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('week during fiscal year with dates', async function () {
      const r = await woql.post(agent, intervalQuery('during',
        dat('2024-03-15'), dat('2024-03-20'),
        dat('2024-01-01'), dat('2025-01-01')))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('FY contains June with dates', async function () {
      const r = await woql.post(agent, intervalQuery('contains',
        dat('2024-01-01'), dat('2025-01-01'),
        dat('2024-06-01'), dat('2024-07-01')))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('Q1 before Q2 with dates', async function () {
      const r = await woql.post(agent, intervalQuery('before',
        dat('2024-01-01'), dat('2024-03-31'),
        dat('2024-04-01'), dat('2024-06-30')))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('Interval (xdd:dateTimeInterval)', function () {
    function datVal (v) { return { '@type': 'xsd:date', '@value': v } }
    function intervalVal (v) { return { '@type': 'xdd:dateTimeInterval', '@value': v } }

    it('constructs interval from start+end dates', async function () {
      const q = {
        '@type': 'Interval',
        start: { '@type': 'DataValue', data: datVal('2025-01-01') },
        end: { '@type': 'DataValue', data: datVal('2025-04-01') },
        interval: { '@type': 'DataValue', variable: 'v:i' },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:i']['@type']).to.equal('xdd:dateTimeInterval')
      expect(r.body.bindings[0]['v:i']['@value']).to.equal('2025-01-01/2025-04-01')
    })
    it('deconstructs interval into start+end dates', async function () {
      const q = {
        '@type': 'Interval',
        start: { '@type': 'DataValue', variable: 'v:s' },
        end: { '@type': 'DataValue', variable: 'v:e' },
        interval: { '@type': 'DataValue', data: intervalVal('2025-01-01/2025-04-01') },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:s']['@value']).to.equal('2025-01-01')
      expect(r.body.bindings[0]['v:e']['@value']).to.equal('2025-04-01')
    })
    it('validates matching start+end+interval', async function () {
      const q = {
        '@type': 'Interval',
        start: { '@type': 'DataValue', data: datVal('2025-01-01') },
        end: { '@type': 'DataValue', data: datVal('2025-04-01') },
        interval: { '@type': 'DataValue', data: intervalVal('2025-01-01/2025-04-01') },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('rejects mismatched start+end+interval', async function () {
      const q = {
        '@type': 'Interval',
        start: { '@type': 'DataValue', data: datVal('2025-01-01') },
        end: { '@type': 'DataValue', data: datVal('2025-06-01') },
        interval: { '@type': 'DataValue', data: intervalVal('2025-01-01/2025-04-01') },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(0)
    })
    it('constructs interval from dateTime endpoints', async function () {
      const dtVal = (v) => ({ '@type': 'xsd:dateTime', '@value': v })
      const q = {
        '@type': 'Interval',
        start: { '@type': 'DataValue', data: dtVal('2025-01-01T09:00:00Z') },
        end: { '@type': 'DataValue', data: dtVal('2025-01-01T17:30:00Z') },
        interval: { '@type': 'DataValue', variable: 'v:i' },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:i']['@value']).to.equal('2025-01-01T09:00:00Z/2025-01-01T17:30:00Z')
    })
    it('constructs mixed date/dateTime interval', async function () {
      const dtVal = (v) => ({ '@type': 'xsd:dateTime', '@value': v })
      const q = {
        '@type': 'Interval',
        start: { '@type': 'DataValue', data: datVal('2025-01-01') },
        end: { '@type': 'DataValue', data: dtVal('2025-04-01T12:00:00Z') },
        interval: { '@type': 'DataValue', variable: 'v:i' },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:i']['@value']).to.equal('2025-01-01/2025-04-01T12:00:00Z')
    })
  })

  describe('IntervalStartDuration', function () {
    function datVal (v) { return { '@type': 'xsd:date', '@value': v } }
    function durVal (v) { return { '@type': 'xsd:duration', '@value': v } }
    function intervalVal (v) { return { '@type': 'xdd:dateTimeInterval', '@value': v } }

    it('extracts start + duration from a 90-day interval', async function () {
      const q = {
        '@type': 'IntervalStartDuration',
        start: { '@type': 'DataValue', variable: 'v:s' },
        duration: { '@type': 'DataValue', variable: 'v:d' },
        interval: { '@type': 'DataValue', data: intervalVal('2025-01-01/2025-04-01') },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:s']['@value']).to.equal('2025-01-01')
      expect(r.body.bindings[0]['v:d']['@value']).to.equal('P90D')
    })
    it('constructs interval from start + P90D duration', async function () {
      const q = {
        '@type': 'IntervalStartDuration',
        start: { '@type': 'DataValue', data: datVal('2025-01-01') },
        duration: { '@type': 'DataValue', data: durVal('P90D') },
        interval: { '@type': 'DataValue', variable: 'v:i' },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:i']['@value']).to.equal('2025-01-01/2025-04-01')
    })
    it('extracts sub-day duration from dateTime interval', async function () {
      const q = {
        '@type': 'IntervalStartDuration',
        start: { '@type': 'DataValue', variable: 'v:s' },
        duration: { '@type': 'DataValue', variable: 'v:d' },
        interval: { '@type': 'DataValue', data: intervalVal('2025-01-01T09:00:00Z/2025-01-01T17:30:00Z') },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:s']['@type']).to.equal('xsd:dateTime')
      expect(r.body.bindings[0]['v:d']['@value']).to.equal('PT8H30M')
    })
  })

  describe('IntervalDurationEnd', function () {
    function datVal (v) { return { '@type': 'xsd:date', '@value': v } }
    function durVal (v) { return { '@type': 'xsd:duration', '@value': v } }
    function intervalVal (v) { return { '@type': 'xdd:dateTimeInterval', '@value': v } }

    it('extracts end + duration from a 90-day interval', async function () {
      const q = {
        '@type': 'IntervalDurationEnd',
        duration: { '@type': 'DataValue', variable: 'v:d' },
        end: { '@type': 'DataValue', variable: 'v:e' },
        interval: { '@type': 'DataValue', data: intervalVal('2025-01-01/2025-04-01') },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:e']['@value']).to.equal('2025-04-01')
      expect(r.body.bindings[0]['v:d']['@value']).to.equal('P90D')
    })
    it('constructs interval from P90D duration + end date', async function () {
      const q = {
        '@type': 'IntervalDurationEnd',
        duration: { '@type': 'DataValue', data: durVal('P90D') },
        end: { '@type': 'DataValue', data: datVal('2025-04-01') },
        interval: { '@type': 'DataValue', variable: 'v:i' },
      }
      const r = await woql.post(agent, q)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:i']['@value']).to.equal('2025-01-01/2025-04-01')
    })
  })

  describe('DayAfter', function () {
    function datVal (v) { return { '@type': 'xsd:date', '@value': v } }
    function dvArg (v) {
      if (typeof v === 'string' && v.startsWith('v:')) return { '@type': 'DataValue', variable: v }
      return { '@type': 'DataValue', data: datVal(v) }
    }
    function dayAfterQuery (date, next) {
      return { '@type': 'DayAfter', date: dvArg(date), next: dvArg(next) }
    }

    it('mid-month: Jan 15 -> Jan 16', async function () {
      const r = await woql.post(agent, dayAfterQuery('2025-01-15', 'v:n'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:n']['@value']).to.equal('2025-01-16')
    })
    it('end-of-month: Jan 31 -> Feb 1', async function () {
      const r = await woql.post(agent, dayAfterQuery('2025-01-31', 'v:n'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:n']['@value']).to.equal('2025-02-01')
    })
    it('end-of-year: Dec 31 -> Jan 1 next year', async function () {
      const r = await woql.post(agent, dayAfterQuery('2025-12-31', 'v:n'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:n']['@value']).to.equal('2026-01-01')
    })
    it('leap Feb 28 -> Feb 29', async function () {
      const r = await woql.post(agent, dayAfterQuery('2024-02-28', 'v:n'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:n']['@value']).to.equal('2024-02-29')
    })
    it('leap Feb 29 -> Mar 1', async function () {
      const r = await woql.post(agent, dayAfterQuery('2024-02-29', 'v:n'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:n']['@value']).to.equal('2024-03-01')
    })
    it('non-leap Feb 28 -> Mar 1', async function () {
      const r = await woql.post(agent, dayAfterQuery('2023-02-28', 'v:n'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:n']['@value']).to.equal('2023-03-01')
    })
    it('bidirectional: given next=Apr 1, deduces date=Mar 31', async function () {
      const r = await woql.post(agent, dayAfterQuery('v:d', '2025-04-01'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:d']['@value']).to.equal('2025-03-31')
    })
  })

  describe('DayBefore', function () {
    function datVal (v) { return { '@type': 'xsd:date', '@value': v } }
    function dvArg (v) {
      if (typeof v === 'string' && v.startsWith('v:')) return { '@type': 'DataValue', variable: v }
      return { '@type': 'DataValue', data: datVal(v) }
    }
    function dayBeforeQuery (date, previous) {
      return { '@type': 'DayBefore', date: dvArg(date), previous: dvArg(previous) }
    }

    it('mid-month: Jan 15 -> Jan 14', async function () {
      const r = await woql.post(agent, dayBeforeQuery('2025-01-15', 'v:p'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:p']['@value']).to.equal('2025-01-14')
    })
    it('start-of-month: Mar 1 -> Feb 28', async function () {
      const r = await woql.post(agent, dayBeforeQuery('2025-03-01', 'v:p'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:p']['@value']).to.equal('2025-02-28')
    })
    it('start-of-year: Jan 1 -> Dec 31 prev year', async function () {
      const r = await woql.post(agent, dayBeforeQuery('2025-01-01', 'v:p'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:p']['@value']).to.equal('2024-12-31')
    })
    it('leap Mar 1 -> Feb 29', async function () {
      const r = await woql.post(agent, dayBeforeQuery('2024-03-01', 'v:p'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:p']['@value']).to.equal('2024-02-29')
    })
    it('bidirectional: given previous=Mar 31, deduces date=Apr 1', async function () {
      const r = await woql.post(agent, dayBeforeQuery('v:d', '2025-03-31'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:d']['@value']).to.equal('2025-04-01')
    })
  })

  describe('Weekday (ISO: Monday=1, Sunday=7)', function () {
    function datVal (v) { return { '@type': 'xsd:date', '@value': v } }
    function dtmVal (v) { return { '@type': 'xsd:dateTime', '@value': v } }
    function weekdayQuery (date, weekday) {
      const dateArg = typeof date === 'string' && date.startsWith('v:')
        ? { '@type': 'DataValue', variable: date }
        : { '@type': 'DataValue', data: date.includes('T') ? dtmVal(date) : datVal(date) }
      const wArg = typeof weekday === 'string' && weekday.startsWith('v:')
        ? { '@type': 'DataValue', variable: weekday }
        : { '@type': 'DataValue', data: weekday }
      return { '@type': 'Weekday', date: dateArg, weekday: wArg }
    }

    it('Monday: 2024-01-01 -> 1', async function () {
      const r = await woql.post(agent, weekdayQuery('2024-01-01', 'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:d']['@value']).to.equal(1)
    })
    it('Sunday: 2024-01-07 -> 7', async function () {
      const r = await woql.post(agent, weekdayQuery('2024-01-07', 'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:d']['@value']).to.equal(7)
    })
    it('Leap day 2024-02-29 -> Thursday (4)', async function () {
      const r = await woql.post(agent, weekdayQuery('2024-02-29', 'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:d']['@value']).to.equal(4)
    })
    it('dateTime: 2024-01-01T12:00:00Z -> Monday (1)', async function () {
      const r = await woql.post(agent, weekdayQuery('2024-01-01T12:00:00Z', 'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:d']['@value']).to.equal(1)
    })
    it('validates: 2024-01-01 is Monday (1)', async function () {
      const r = await woql.post(agent, weekdayQuery('2024-01-01', { '@type': 'xsd:integer', '@value': 1 }))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validation fails: 2024-01-01 is not Tuesday (2)', async function () {
      const r = await woql.post(agent, weekdayQuery('2024-01-01', { '@type': 'xsd:integer', '@value': 2 }))
      expect(r.body.bindings).to.have.lengthOf(0)
    })
  })

  describe('WeekdaySundayStart (US: Sunday=1, Saturday=7)', function () {
    function datVal (v) { return { '@type': 'xsd:date', '@value': v } }
    function sundayStartQuery (date, weekday) {
      const dateArg = { '@type': 'DataValue', data: datVal(date) }
      const wArg = typeof weekday === 'string' && weekday.startsWith('v:')
        ? { '@type': 'DataValue', variable: weekday }
        : { '@type': 'DataValue', data: weekday }
      return { '@type': 'WeekdaySundayStart', date: dateArg, weekday: wArg }
    }

    it('Sunday: 2024-01-07 -> 1', async function () {
      const r = await woql.post(agent, sundayStartQuery('2024-01-07', 'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:d']['@value']).to.equal(1)
    })
    it('Saturday: 2024-01-06 -> 7', async function () {
      const r = await woql.post(agent, sundayStartQuery('2024-01-06', 'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:d']['@value']).to.equal(7)
    })
    it('Monday: 2024-01-01 -> 2', async function () {
      const r = await woql.post(agent, sundayStartQuery('2024-01-01', 'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:d']['@value']).to.equal(2)
    })
  })

  describe('IsoWeek (ISO 8601 week-date)', function () {
    function datVal (v) { return { '@type': 'xsd:date', '@value': v } }
    function dtmVal (v) { return { '@type': 'xsd:dateTime', '@value': v } }
    function isoWeekQuery (date, year, week) {
      const dateArg = typeof date === 'string' && date.includes('T')
        ? { '@type': 'DataValue', data: dtmVal(date) }
        : { '@type': 'DataValue', data: datVal(date) }
      const yArg = typeof year === 'string' && year.startsWith('v:')
        ? { '@type': 'DataValue', variable: year }
        : { '@type': 'DataValue', data: year }
      const wArg = typeof week === 'string' && week.startsWith('v:')
        ? { '@type': 'DataValue', variable: week }
        : { '@type': 'DataValue', data: week }
      return { '@type': 'IsoWeek', date: dateArg, year: yArg, week: wArg }
    }

    it('2024-01-01 -> week 1 of 2024', async function () {
      const r = await woql.post(agent, isoWeekQuery('2024-01-01', 'v:y', 'v:w'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:y']['@value']).to.equal(2024)
      expect(r.body.bindings[0]['v:w']['@value']).to.equal(1)
    })
    it('2024-12-30 -> week 1 of 2025 (year boundary)', async function () {
      const r = await woql.post(agent, isoWeekQuery('2024-12-30', 'v:y', 'v:w'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:y']['@value']).to.equal(2025)
      expect(r.body.bindings[0]['v:w']['@value']).to.equal(1)
    })
    it('2023-01-01 -> week 52 of 2022', async function () {
      const r = await woql.post(agent, isoWeekQuery('2023-01-01', 'v:y', 'v:w'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:y']['@value']).to.equal(2022)
      expect(r.body.bindings[0]['v:w']['@value']).to.equal(52)
    })
    it('2020-12-31 -> week 53 of 2020 (53-week year)', async function () {
      const r = await woql.post(agent, isoWeekQuery('2020-12-31', 'v:y', 'v:w'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:y']['@value']).to.equal(2020)
      expect(r.body.bindings[0]['v:w']['@value']).to.equal(53)
    })
    it('2024-06-15 -> week 24 of 2024', async function () {
      const r = await woql.post(agent, isoWeekQuery('2024-06-15', 'v:y', 'v:w'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:y']['@value']).to.equal(2024)
      expect(r.body.bindings[0]['v:w']['@value']).to.equal(24)
    })
    it('dateTime: 2024-06-15T09:30:00Z -> week 24 of 2024', async function () {
      const r = await woql.post(agent, isoWeekQuery('2024-06-15T09:30:00Z', 'v:y', 'v:w'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0]['v:y']['@value']).to.equal(2024)
      expect(r.body.bindings[0]['v:w']['@value']).to.equal(24)
    })
    it('validates: 2024-01-01 is week 1 of 2024', async function () {
      const r = await woql.post(agent, isoWeekQuery('2024-01-01', { '@type': 'xsd:integer', '@value': 2024 }, { '@type': 'xsd:integer', '@value': 1 }))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validation fails: 2024-01-01 is not week 2', async function () {
      const r = await woql.post(agent, isoWeekQuery('2024-01-01', { '@type': 'xsd:integer', '@value': 2024 }, { '@type': 'xsd:integer', '@value': 2 }))
      expect(r.body.bindings).to.have.lengthOf(0)
    })
  })

  function dateDurationQuery (start, end, duration) {
    const dv = (v) => {
      if (typeof v === 'string' && v.startsWith('v:')) {
        return { '@type': 'DataValue', variable: v.slice(2) }
      }
      if (typeof v === 'object' && v['@type']) {
        return { '@type': 'DataValue', data: v }
      }
      return { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': v } }
    }
    return { '@type': 'DateDuration', start: dv(start), end: dv(end), duration: dv(duration) }
  }

  describe('DateDuration', function () {
    it('computes duration: 2024-01-01 to 2024-04-01 = P91D (leap)', async function () {
      const r = await woql.post(agent, dateDurationQuery(
        { '@type': 'xsd:date', '@value': '2024-01-01' },
        { '@type': 'xsd:date', '@value': '2024-04-01' },
        'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('P91D')
    })
    it('computes duration: 2025-01-01 to 2025-04-01 = P90D (non-leap)', async function () {
      const r = await woql.post(agent, dateDurationQuery(
        { '@type': 'xsd:date', '@value': '2025-01-01' },
        { '@type': 'xsd:date', '@value': '2025-04-01' },
        'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('P90D')
    })
    it('EOM add: Jan 31 + P1M = Feb 29 (leap)', async function () {
      const r = await woql.post(agent, dateDurationQuery(
        { '@type': 'xsd:date', '@value': '2020-01-31' },
        'v:e',
        { '@type': 'xsd:duration', '@value': 'P1M' }))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].e['@value']).to.equal('2020-02-29')
    })
    it('EOM add: Feb 29 + P1M = Mar 31', async function () {
      const r = await woql.post(agent, dateDurationQuery(
        { '@type': 'xsd:date', '@value': '2020-02-29' },
        'v:e',
        { '@type': 'xsd:duration', '@value': 'P1M' }))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].e['@value']).to.equal('2020-03-31')
    })
    it('EOM subtract: Mar 31 - P1M = Feb 29 (leap)', async function () {
      const r = await woql.post(agent, dateDurationQuery(
        'v:s',
        { '@type': 'xsd:date', '@value': '2020-03-31' },
        { '@type': 'xsd:duration', '@value': 'P1M' }))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].s['@value']).to.equal('2020-02-29')
    })
    it('EOM reversibility: Feb 29 - P1M = Jan 31', async function () {
      const r = await woql.post(agent, dateDurationQuery(
        'v:s',
        { '@type': 'xsd:date', '@value': '2020-02-29' },
        { '@type': 'xsd:duration', '@value': 'P1M' }))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].s['@value']).to.equal('2020-01-31')
    })
    it('datetime with time: computes PT9H30M', async function () {
      const r = await woql.post(agent, dateDurationQuery(
        { '@type': 'xsd:dateTime', '@value': '2024-01-01T08:00:00Z' },
        { '@type': 'xsd:dateTime', '@value': '2024-01-01T17:30:00Z' },
        'v:d'))
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].d['@value']).to.equal('PT9H30M')
    })
    it('validates: consistent start+end+duration', async function () {
      const r = await woql.post(agent, dateDurationQuery(
        { '@type': 'xsd:date', '@value': '2024-01-01' },
        { '@type': 'xsd:date', '@value': '2024-04-01' },
        { '@type': 'xsd:duration', '@value': 'P91D' }))
      expect(r.body.bindings).to.have.lengthOf(1)
    })
    it('validation fails: inconsistent start+end+duration', async function () {
      const r = await woql.post(agent, dateDurationQuery(
        { '@type': 'xsd:date', '@value': '2024-01-01' },
        { '@type': 'xsd:date', '@value': '2024-04-01' },
        { '@type': 'xsd:duration', '@value': 'P90D' }))
      expect(r.body.bindings).to.have.lengthOf(0)
    })
  })
})
