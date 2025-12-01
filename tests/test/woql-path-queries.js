const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('woql-path-queries', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent, { schema: false })
  })

  after(async function () {
    await db.delete(agent)
  })

  // Helper function to set up test data
  async function setupChainData () {
    const setup = {
      '@type': 'And',
      and: [
        {
          '@type': 'AddTriple',
          subject: { '@type': 'NodeValue', node: 'a' },
          predicate: { '@type': 'NodeValue', node: 'p1' },
          object: { '@type': 'Value', node: 'b' },
        },
        {
          '@type': 'AddTriple',
          subject: { '@type': 'NodeValue', node: 'b' },
          predicate: { '@type': 'NodeValue', node: 'p2' },
          object: { '@type': 'Value', node: 'c' },
        },
        {
          '@type': 'AddTriple',
          subject: { '@type': 'NodeValue', node: 'c' },
          predicate: { '@type': 'NodeValue', node: 'p3' },
          object: { '@type': 'Value', node: 'd' },
        },
      ],
    }
    await woql.post(agent, setup)
  }

  it('star with ANY predicate traverses all levels', async function () {
    await setupChainData()

    // Query: traverse with star operator and ANY predicate
    // Note: To use ANY predicate, omit the 'predicate' field entirely
    const query = {
      '@type': 'Path',
      subject: { '@type': 'NodeValue', node: 'a' },
      pattern: {
        '@type': 'PathStar',
        star: {
          '@type': 'PathPredicate',
          // No 'predicate' field = ANY predicate
        },
      },
      object: { '@type': 'NodeValue', variable: 'X' },
    }

    const r = await woql.post(agent, query)

    // Should find: a (0 hops), b (1 hop), c (2 hops), d (3 hops) = 4 results
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(4)

    const nodes = r.body.bindings.map(b => b.X).sort()
    expect(nodes).to.include('a')
    expect(nodes).to.include('b')
    expect(nodes).to.include('c')
    expect(nodes).to.include('d')
  })

  it('star with explicit predicate choice traverses all levels', async function () {
    // Query: traverse with explicit choice of predicates
    const query = {
      '@type': 'Path',
      subject: { '@type': 'NodeValue', node: 'a' },
      pattern: {
        '@type': 'PathStar',
        star: {
          '@type': 'PathOr',
          or: [
            {
              '@type': 'PathPredicate',
              predicate: '@schema:p1',
            },
            {
              '@type': 'PathPredicate',
              predicate: '@schema:p2',
            },
            {
              '@type': 'PathPredicate',
              predicate: '@schema:p3',
            },
          ],
        },
      },
      object: { '@type': 'NodeValue', variable: 'X' },
    }

    const r = await woql.post(agent, query)

    // Should find all 4 nodes
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(4)

    const nodes = r.body.bindings.map(b => b.X).sort()
    expect(nodes).to.include('a')
    expect(nodes).to.include('b')
    expect(nodes).to.include('c')
    expect(nodes).to.include('d')
  })

  it('plus with ANY predicate traverses all levels (at least one hop)', async function () {
    // Plus operator requires at least one hop, so starting node is excluded
    const query = {
      '@type': 'Path',
      subject: { '@type': 'NodeValue', node: 'a' },
      pattern: {
        '@type': 'PathPlus',
        plus: {
          '@type': 'PathPredicate',
          // No 'predicate' field = ANY predicate
        },
      },
      object: { '@type': 'NodeValue', variable: 'X' },
    }

    const r = await woql.post(agent, query)

    // Should find: b (1 hop), c (2 hops), d (3 hops) = 3 results (excludes 'a')
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(3)

    const nodes = r.body.bindings.map(b => b.X).sort()
    expect(nodes).to.not.include('a')
    expect(nodes).to.include('b')
    expect(nodes).to.include('c')
    expect(nodes).to.include('d')
  })

  it('times{1,2} with ANY predicate matches 1-2 hops', async function () {
    // Times operator with range {1,2} matches exactly 1 or 2 hops
    const query = {
      '@type': 'Path',
      subject: { '@type': 'NodeValue', node: 'a' },
      pattern: {
        '@type': 'PathTimes',
        from: 1,
        to: 2,
        times: {
          '@type': 'PathPredicate',
          // No 'predicate' field = ANY predicate
        },
      },
      object: { '@type': 'NodeValue', variable: 'X' },
    }

    const r = await woql.post(agent, query)

    // Should find: b (1 hop), c (2 hops) = 2 results
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(2)

    const nodes = r.body.bindings.map(b => b.X).sort()
    expect(nodes).to.not.include('a')
    expect(nodes).to.include('b')
    expect(nodes).to.include('c')
    expect(nodes).to.not.include('d')
  })

  it('times{2,3} with ANY predicate matches 2-3 hops', async function () {
    // Times operator with range {2,3} matches exactly 2 or 3 hops
    const query = {
      '@type': 'Path',
      subject: { '@type': 'NodeValue', node: 'a' },
      pattern: {
        '@type': 'PathTimes',
        from: 2,
        to: 3,
        times: {
          '@type': 'PathPredicate',
          // No 'predicate' field = ANY predicate
        },
      },
      object: { '@type': 'NodeValue', variable: 'X' },
    }

    const r = await woql.post(agent, query)

    // Should find: c (2 hops), d (3 hops) = 2 results
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(2)

    const nodes = r.body.bindings.map(b => b.X).sort()
    expect(nodes).to.not.include('a')
    expect(nodes).to.not.include('b')
    expect(nodes).to.include('c')
    expect(nodes).to.include('d')
  })

  // BUG: times{0,0} should return the starting node (follow edge 0 times)
  it('times{0,0} returns starting node only (zero hops)', async function () {
    // Times operator with range {0,0} means follow the edge exactly 0 times
    // This should return only the starting node itself
    const query = {
      '@type': 'Path',
      subject: { '@type': 'NodeValue', node: 'a' },
      pattern: {
        '@type': 'PathTimes',
        from: 0,
        to: 0,
        times: {
          '@type': 'PathPredicate',
          // No 'predicate' field = ANY predicate
        },
      },
      object: { '@type': 'NodeValue', variable: 'X' },
    }

    const r = await woql.post(agent, query)

    // Should find only: a (0 hops) = 1 result
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)

    const nodes = r.body.bindings.map(b => b.X)
    expect(nodes).to.include('a')
    expect(nodes).to.not.include('b')
    expect(nodes).to.not.include('c')
    expect(nodes).to.not.include('d')
  })

  it('times{0,1} returns starting node and one hop', async function () {
    // Times operator with range {0,1} means follow 0 or 1 times
    const query = {
      '@type': 'Path',
      subject: { '@type': 'NodeValue', node: 'a' },
      pattern: {
        '@type': 'PathTimes',
        from: 0,
        to: 1,
        times: {
          '@type': 'PathPredicate',
          // No 'predicate' field = ANY predicate
        },
      },
      object: { '@type': 'NodeValue', variable: 'X' },
    }

    const r = await woql.post(agent, query)

    // Should find: a (0 hops), b (1 hop) = 2 results
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(2)

    const nodes = r.body.bindings.map(b => b.X).sort()
    expect(nodes).to.include('a')
    expect(nodes).to.include('b')
    expect(nodes).to.not.include('c')
    expect(nodes).to.not.include('d')
  })

  it('times{0,2} returns starting node and up to two hops', async function () {
    // Times operator with range {0,2} means follow 0, 1, or 2 times
    const query = {
      '@type': 'Path',
      subject: { '@type': 'NodeValue', node: 'a' },
      pattern: {
        '@type': 'PathTimes',
        from: 0,
        to: 2,
        times: {
          '@type': 'PathPredicate',
          // No 'predicate' field = ANY predicate
        },
      },
      object: { '@type': 'NodeValue', variable: 'X' },
    }

    const r = await woql.post(agent, query)

    // Should find: a (0 hops), b (1 hop), c (2 hops) = 3 results
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(3)

    const nodes = r.body.bindings.map(b => b.X).sort()
    expect(nodes).to.include('a')
    expect(nodes).to.include('b')
    expect(nodes).to.include('c')
    expect(nodes).to.not.include('d')
  })
})
