const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('woql-set-operations', function () {
  this.timeout(200000)

  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('set_difference', function () {
    it('computes difference between two lists', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 3 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 4 } },
              ],
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 4 } },
              ],
            },
          },
          {
            '@type': 'SetDifference',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'Diff' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const diff = result.body.bindings[0].Diff
      const diffValues = diff.map(v => v['@value'])
      expect(diffValues).to.deep.equal([1, 3])
    })

    it('returns empty list when first list is subset of second', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 3 } },
              ],
            },
          },
          {
            '@type': 'SetDifference',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'Diff' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const diff = result.body.bindings[0].Diff
      expect(diff).to.deep.equal([])
    })

    it('handles empty lists', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: { '@type': 'Value', list: [] },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
              ],
            },
          },
          {
            '@type': 'SetDifference',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'Diff' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const diff = result.body.bindings[0].Diff
      expect(diff).to.deep.equal([])
    })
  })

  describe('set_intersection', function () {
    it('computes intersection of two lists', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 3 } },
              ],
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 3 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 4 } },
              ],
            },
          },
          {
            '@type': 'SetIntersection',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'Common' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const common = result.body.bindings[0].Common
      const commonValues = common.map(v => v['@value'])
      expect(commonValues).to.deep.equal([2, 3])
    })

    it('returns empty list when no common elements', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 3 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 4 } },
              ],
            },
          },
          {
            '@type': 'SetIntersection',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'Common' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const common = result.body.bindings[0].Common
      expect(common).to.deep.equal([])
    })
  })

  describe('set_union', function () {
    it('computes union of two lists', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 3 } },
              ],
            },
          },
          {
            '@type': 'SetUnion',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'All' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const all = result.body.bindings[0].All
      const allValues = all.map(v => v['@value'])
      expect(allValues).to.deep.equal([1, 2, 3])
    })

    it('removes duplicates', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'SetUnion',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'All' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const all = result.body.bindings[0].All
      const allValues = all.map(v => v['@value'])
      expect(allValues).to.deep.equal([1, 2])
    })
  })

  describe('set_member', function () {
    it('checks membership in a set efficiently', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'MySet' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 3 } },
              ],
            },
          },
          {
            '@type': 'SetMember',
            element: { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
            set: { '@type': 'Value', variable: 'MySet' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
    })

    it('fails for non-member', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'MySet' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 3 } },
              ],
            },
          },
          {
            '@type': 'SetMember',
            element: { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 5 } },
            set: { '@type': 'Value', variable: 'MySet' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(0)
    })
  })

  describe('list_to_set', function () {
    it('converts list to set removing duplicates and sorting', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'MyList' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 3 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
              ],
            },
          },
          {
            '@type': 'ListToSet',
            list: { '@type': 'Value', variable: 'MyList' },
            set: { '@type': 'Value', variable: 'MySet' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const mySet = result.body.bindings[0].MySet
      const mySetValues = mySet.map(v => v['@value'])
      expect(mySetValues).to.deep.equal([1, 2, 3])
    })
  })

  describe('incommensurable types behavior', function () {
    it('treats same value with different types as distinct elements in set_difference', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'SetDifference',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'Diff' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const diff = result.body.bindings[0].Diff
      // Both integers remain because decimals are different types
      expect(diff.length).to.equal(2)
      const diffValues = diff.map(v => v['@value'])
      expect(diffValues).to.deep.equal([1, 2])
    })

    it('treats same value with different types as distinct in set_intersection', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'SetIntersection',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'Common' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const common = result.body.bindings[0].Common
      // No common elements because types differ
      expect(common).to.deep.equal([])
    })

    it('treats same value with different types as distinct in set_union', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListA' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
              ],
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'ListB' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': 1 } },
              ],
            },
          },
          {
            '@type': 'SetUnion',
            list_a: { '@type': 'Value', variable: 'ListA' },
            list_b: { '@type': 'Value', variable: 'ListB' },
            result: { '@type': 'Value', variable: 'All' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const all = result.body.bindings[0].All
      // Both elements present because types differ
      expect(all.length).to.equal(2)
    })

    it('set_member requires exact type match', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'MySet' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 2 } },
              ],
            },
          },
          {
            '@type': 'SetMember',
            element: { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': 1 } },
            set: { '@type': 'Value', variable: 'MySet' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      // No match because decimal 1 is not in set of integers
      expect(result.body.bindings).to.have.length(0)
    })

    it('list_to_set preserves distinct types', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'MyList' },
            right: {
              '@type': 'Value',
              list: [
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': 1 } },
                { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': 1 } },
              ],
            },
          },
          {
            '@type': 'ListToSet',
            list: { '@type': 'Value', variable: 'MyList' },
            set: { '@type': 'Value', variable: 'MySet' },
          },
        ],
      }

      const result = await woql.post(agent, query)
      expect(result.status).to.equal(200)
      expect(result.body.bindings).to.have.length(1)
      const mySet = result.body.bindings[0].MySet
      // Two elements: integer 1 and decimal 1 (duplicate integer removed)
      expect(mySet.length).to.equal(2)
    })
  })
})
