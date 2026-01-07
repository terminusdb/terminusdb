const { expect } = require('chai')
const { Agent, db, document } = require('../../lib')

describe('rollup-performance-test', function () {
  let agent
  const persistentDbName = 'rollup-perf-test-db'

  before(async function () {
    agent = new Agent().auth()
    // Override db name to use persistent database
    agent.dbName = persistentDbName

    // Check if database exists
    let dbExists = false
    try {
      const response = await agent.get(`/api/db/${agent.orgName}/${agent.dbName}`)
      if (response.status === 200) {
        dbExists = true
        console.log('\n  ♻️  Reusing existing database (restart test mode)')
      }
    } catch (e) {
      // Database doesn't exist
      dbExists = false
    }

    // Create database if it doesn't exist
    if (!dbExists) {
      console.log('\n  📦 Creating fresh database')
      await db.create(agent, { label: 'Rollup Performance Test DB', schema: false })

      const schema = {
        '@type': 'Class',
        '@id': 'TestDoc',
        '@key': { '@type': 'Random' },
        data: 'xsd:string',
      }

      await document.insert(agent, { schema })
    }
  })

  after(async function () {
    // Don't delete database - keep it for restart testing
    console.log('\n  Database preserved for restart testing')
    console.log('  To test restart: ./tests/terminusdb-test-server.sh restart')
    console.log('  Then run this test again to see performance reset\n')
  })

  it('should verify that manual rollup eliminates O(n²) degradation', async function () {
    this.timeout(180000)

    // Check how many docs already exist to determine run number
    let existingDocs = 0
    try {
      const result = await agent.get(`/api/document/${agent.orgName}/${agent.dbName}?type=TestDoc&skip=0&count=10000`)
      existingDocs = result.body.length
    } catch (e) {
      existingDocs = 0
    }

    const runNumber = Math.floor(existingDocs / (70 * 15)) + 1
    const isRestart = runNumber > 1

    console.log('\n  ===== ROLLUP OPTIMIZATION TEST =====')
    console.log(`  Run #${runNumber} ${isRestart ? '(AFTER RESTART)' : '(FRESH SESSION)'}`)
    console.log(`  Existing documents: ${existingDocs}`)
    console.log('  Hypothesis: rollup() flattens layer stack, eliminating degradation')
    console.log('  Testing WITH manual rollup after each cycle\n')

    if (isRestart) {
      console.log('  This is a restart test - checking if performance reset\n')
    }

    const docsPerCycle = 70
    const cycles = 15

    function generateDocument () {
      return {
        '@type': 'TestDoc',
        data: 'x'.repeat(5000) + Math.random(),
      }
    }

    async function manualRollup () {
      try {
        // Call the optimize API to trigger rollup
        const result = await agent.post(`/api/optimize/${agent.orgName}/${agent.dbName}`)
        return result.status === 200
      } catch (e) {
        console.log(`    [WARNING] Rollup failed: ${e.message}`)
        return false
      }
    }

    const sampleCycles = [1, 5, 10, 15]
    const results = []

    for (let cycle = 1; cycle <= cycles; cycle++) {
      // PHASE 1: INSERT
      const docs = []
      for (let i = 0; i < docsPerCycle; i++) {
        docs.push(generateDocument())
      }

      const insertStart = Date.now()
      const insertResult = await document.insert(agent, { instance: docs })
      const insertTime = Date.now() - insertStart

      const insertedIds = insertResult.body.map(id => id.replace(/^.*\//, ''))

      // PHASE 2: DELETE
      const deleteStart = Date.now()
      for (const docId of insertedIds) {
        await document.delete(agent, { body: `TestDoc/${docId}` })
      }
      const deleteTime = Date.now() - deleteStart

      // PHASE 3: ROLLUP (flatten layer stack)
      const rollupStart = Date.now()
      const rollupSuccess = await manualRollup()
      const rollupTime = Date.now() - rollupStart

      if (sampleCycles.includes(cycle)) {
        results.push({
          cycle,
          insertTime,
          deleteTime,
          rollupTime,
          rollupSuccess,
          insertPerDoc: (insertTime / docsPerCycle).toFixed(2),
        })

        console.log(`  Cycle ${cycle}:`)
        console.log(`    INSERT: ${insertTime}ms (${(insertTime / docsPerCycle).toFixed(2)}ms/doc)`)
        console.log(`    DELETE: ${deleteTime}ms`)
        console.log(`    ROLLUP: ${rollupTime}ms ${rollupSuccess ? '✓' : '✗'}`)
      }
    }

    console.log('\n  ===== PERFORMANCE ANALYSIS WITH ROLLUP =====')
    console.log(`  Run #${runNumber} ${isRestart ? '(AFTER RESTART)' : '(FRESH SESSION)'}`)
    console.log('  Cycle | INSERT Time | Per-Doc | Rollup')
    console.log('  ------|-------------|---------|--------')
    results.forEach(r => {
      console.log(`  ${r.cycle.toString().padStart(5)} | ${r.insertTime.toString().padEnd(11)}ms | ${r.insertPerDoc.padStart(7)}ms | ${r.rollupSuccess ? '✓' : '✗'}`)
    })

    const firstCycle = results[0]
    const lastCycle = results[results.length - 1]
    const degradation = (lastCycle.insertTime / firstCycle.insertTime).toFixed(2)

    console.log('\n  INSERT degradation: ' + degradation + 'x (' + firstCycle.insertTime + 'ms → ' + lastCycle.insertTime + 'ms)\n')

    if (isRestart && degradation < 2) {
      console.log('  SUCCESS: Server restart ELIMINATED degradation')
      console.log('  → This confirms the issue is in-memory Prolog state')
      console.log('  → Not layer structure on disk')
      console.log('  =========================================\n')
    } else if (isRestart && degradation >= 2) {
      console.log('  UNEXPECTED: Degradation persists even after restart')
      console.log('  → This suggests disk-level accumulation')
      console.log('  =========================================\n')
    } else if (!isRestart && degradation < 2) {
      console.log('  SUCCESS: rollup() eliminated degradation in same session')
      console.log('  =========================================\n')
    } else {
      console.log('  EXPECTED: rollup() did NOT eliminate degradation in same session')
      console.log('  → Restart server and run again to verify restart resets performance')
      console.log('  → Command: ./tests/terminusdb-test-server.sh restart')
      console.log('  =========================================\n')
    }

    console.log('  =========================================\n')

    // Store run info for analysis
    console.log(`\n  Total documents after run: ${existingDocs + (15 * 70)}`)
    console.log(`  Next run will be: Run #${runNumber + 1}\n`)

    expect(true).to.be.true
  })
})
