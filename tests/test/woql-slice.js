/**
 * Tests for WOQL slice operator
 *
 * slice(List, Result, Start, End?) - Extract contiguous subsequence from a list
 *
 * Semantics (JavaScript slice() compatible):
 * - Start/End are 0-indexed
 * - End is exclusive (like JS slice)
 * - Negative indices count from end (-1 = last element)
 * - Out-of-bounds indices are clamped
 * - End is optional (defaults to list length)
 * - Inverted indices (start >= end after normalization) should fail
 * - Non-list input should fail
 * - Non-integer indices should fail
 */

const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

// Helper to create typed value for assertions
const str = (v) => ({ '@type': 'xsd:string', '@value': v })
const int = (v) => ({ '@type': 'xsd:integer', '@value': v })

describe('woql-slice', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Basic Slicing', function () {
    it('slice([A, B, C, D], 0, 2) returns [A, B]', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'D' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 0 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 2 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('A'), str('B')])
    })
  })

  describe('Single Element', function () {
    it('slice([A, B, C, D], 1, 2) returns [B]', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'D' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 1 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 2 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('B')])
    })
  })

  describe('Negative Indices', function () {
    it('slice([A, B, C, D], -2, -1) returns [C] (second-to-last to last-exclusive)', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'D' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': -2 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': -1 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('C')])
    })

    it('slice([A, B, C, D], -3, 4) returns [B, C, D] (negative start, positive end)', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'D' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': -3 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 4 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('B'), str('C'), str('D')])
    })
  })

  describe('Out-of-Bounds (clamped)', function () {
    it('slice([A, B, C], 1, 10) returns [B, C] (end clamped to length)', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 1 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 10 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('B'), str('C')])
    })

    it('slice([A, B, C], -10, 2) returns [A, B] (start clamped to 0)', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': -10 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 2 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('A'), str('B')])
    })
  })

  describe('Empty Slice', function () {
    it('slice([A, B, C], 2, 2) returns [] (start equals end)', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 2 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 2 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([])
    })
  })

  describe('Empty List', function () {
    it('slice([], 0, 1) returns []', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 0 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 1 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([])
    })
  })

  describe('Full Range', function () {
    it('slice([A, B, C, D], 0, 4) returns [A, B, C, D]', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'D' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 0 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 4 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('A'), str('B'), str('C'), str('D')])
    })
  })

  describe('Optional End Parameter', function () {
    it('slice([A, B, C, D], 1) returns [B, C, D] (omitted end = rest of list)', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'D' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 1 } },
        // end is omitted
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('B'), str('C'), str('D')])
    })

    it('slice([A, B, C, D], -2) returns [C, D] (negative start, omitted end)', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'D' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': -2 } },
        // end is omitted
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('C'), str('D')])
    })
  })

  describe('Edge Cases (JavaScript slice semantics)', function () {
    it('slice([A, B, C], 2, 1) returns [] (inverted indices = empty per JS semantics)', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'A' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'B' } },
            { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'C' } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 2 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 1 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([])
    })

    it('slice("string", 0, 2) returns empty bindings (non-list input fails silently)', async function () {
      const query = {
        '@type': 'Slice',
        list: { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'string' } },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 0 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 2 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(0)
    })
  })

  describe('Variable List Input', function () {
    it('slice with list from variable binding works', async function () {
      // Use And to first bind a variable to a list, then slice it
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'MyList' },
            right: {
              '@type': 'DataValue',
              list: [
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'X' } },
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Y' } },
                { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'Z' } },
              ],
            },
          },
          {
            '@type': 'Slice',
            list: { '@type': 'DataValue', variable: 'MyList' },
            result: { '@type': 'DataValue', variable: 'Result' },
            start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 1 } },
            end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 3 } },
          },
        ],
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([str('Y'), str('Z')])
    })
  })

  describe('Numeric List Elements', function () {
    it('slice works with integer list elements', async function () {
      const query = {
        '@type': 'Slice',
        list: {
          '@type': 'DataValue',
          list: [
            { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 10 } },
            { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 20 } },
            { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 30 } },
            { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 40 } },
          ],
        },
        result: { '@type': 'DataValue', variable: 'Result' },
        start: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 1 } },
        end: { '@type': 'DataValue', data: { '@type': 'xsd:integer', '@value': 3 } },
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
      expect(r.body.bindings[0].Result).to.deep.equal([int(20), int(30)])
    })
  })
})
