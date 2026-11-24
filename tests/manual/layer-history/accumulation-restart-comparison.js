const { expect } = require('chai')
const { Agent, db, document } = require('../../../tests/lib')

describe('restart-comparison', function () {
  let agent
  const dbName = 'restart-test-db'

  before(async function () {
    agent = new Agent().auth()
    // Use a fixed database name so we can reuse it after restart
    agent.dbName = dbName
  })

  after(async function () {
    // Don't delete - we want to reuse after restart
  })

  it('should measure INSERT timing to detect in-memory cache accumulation', async function () {
    this.timeout(180000)

    console.log('\n  ===== IN-MEMORY CACHE ACCUMULATION TEST =====')
    console.log('  This test measures INSERT timing across cycles.')
    console.log('  Run this test, then restart server (NO --clean), then run again.')
    console.log('  If degradation resets after restart, the issue is in-memory caching.\n')

    // Check if database exists, create if not
    let dbExists = false
    try {
      const existsResult = await agent.get(`/api/db/admin/${dbName}`)
      dbExists = existsResult.status === 200
    } catch (e) {
      dbExists = false
    }

    if (dbExists) {
      console.log(`  Using existing database: ${dbName}`)
    } else {
      console.log(`  Creating new database: ${dbName}`)
      await db.create(agent)

      const schema = [
        {
          '@type': 'Class',
          '@id': 'TestDoc',
          '@key': { '@type': 'Random' },
          data: 'xsd:string',
        },
      ]
      await document.insert(agent, { schema })
    }

    const docsPerCycle = 70
    const cycles = 10

    function generateDocument () {
      return {
        '@type': 'TestDoc',
        data: 'x'.repeat(1000) + Math.random(),
      }
    }

    const results = []

    for (let cycle = 1; cycle <= cycles; cycle++) {
      // INSERT
      const docs = []
      for (let i = 0; i < docsPerCycle; i++) {
        docs.push(generateDocument())
      }

      const insertStart = Date.now()
      const insertResult = await document.insert(agent, { instance: docs })
      const insertTime = Date.now() - insertStart

      const insertedIds = insertResult.body.map(id => id.replace(/^.*\//, ''))

      // DELETE
      const deleteStart = Date.now()
      for (const docId of insertedIds) {
        await document.delete(agent, { body: `TestDoc/${docId}` })
      }
      const deleteTime = Date.now() - deleteStart

      // OPTIMIZE
      const optimizeStart = Date.now()
      try {
        await agent.post(`/api/optimize/${agent.orgName}/${dbName}`)
      } catch (e) {
        console.log(`    [WARNING] Optimize failed: ${e.message}`)
      }
      const optimizeTime = Date.now() - optimizeStart

      results.push({ cycle, insertTime, deleteTime, optimizeTime })

      console.log(`  Cycle ${cycle}: INSERT=${insertTime}ms, DELETE=${deleteTime}ms, OPTIMIZE=${optimizeTime}ms`)
    }

    console.log('\n  ===== RESULTS =====')
    const firstInsert = results[0].insertTime
    const lastInsert = results[results.length - 1].insertTime
    const degradation = (lastInsert / firstInsert).toFixed(2)

    console.log(`  First INSERT: ${firstInsert}ms`)
    console.log(`  Last INSERT: ${lastInsert}ms`)
    console.log(`  Degradation: ${degradation}x`)

    if (parseFloat(degradation) > 2) {
      console.log('\n  ⚠️  SIGNIFICANT DEGRADATION DETECTED')
      console.log('  → If this is after a fresh start, in-memory cache is accumulating')
      console.log('  → If degradation resets after restart, confirms in-memory issue')
    } else {
      console.log('\n  ✓ Minimal degradation')
      console.log('  → If this is after restart, confirms the fix works')
    }

    console.log('\n  To test restart behavior:')
    console.log('  1. Note the degradation above')
    console.log('  2. Run: ./tests/terminusdb-test-server.sh restart')
    console.log('  3. Run this test again')
    console.log('  4. Compare first cycle INSERT times')
    console.log('  =====================================\n')

    expect(true).to.equal(true)
  })
})
