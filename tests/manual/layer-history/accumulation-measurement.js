const { expect } = require('chai')
const { Agent, db, document } = require('../../lib')

describe('accumulation-measurement', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
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
  })

  after(async function () {
    await db.delete(agent)
  })

  it('should measure what accumulates across INSERT+DELETE cycles', async function () {
    this.timeout(300000)

    console.log('\n  ===== ACCUMULATION MEASUREMENT WITH ROLLUP (ONE-BY-ONE INSERTS) =====')
    console.log('  Pattern: INSERT 70 docs (1 by 1) → DELETE 70 docs (1 by 1) → ROLLUP (repeat 20 cycles)')
    console.log('  Measuring: doc count, INSERT timing, DELETE timing, ROLLUP timing\n')

    const docsPerCycle = 70

    function generateDocument () {
      return {
        '@type': 'TestDoc',
        data: 'x'.repeat(500) + Math.random(),
      }
    }

    async function countDocs () {
      try {
        const countQuery = {
          '@type': 'Count',
          query: {
            '@type': 'Triple',
            subject: { '@type': 'Variable', name: 'Doc' },
            predicate: { '@type': 'NodeValue', node: 'rdf:type' },
            object: { '@type': 'NodeValue', node: 'TestDoc' },
          },
        }

        const result = await agent.post(`/api/woql/${agent.orgName}/${agent.dbName}`)
          .send({ query: countQuery })

        if (result.body && result.body.bindings && result.body.bindings.length > 0) {
          return parseInt(result.body.bindings[0].Count['@value'])
        }
      } catch (e) {
        console.log(`    [WARNING] Could not count docs: ${e.message}`)
      }
      return -1
    }

    const results = []
    const sampleCycles = [1, 5, 10, 15, 20]

    for (let cycle = 1; cycle <= 20; cycle++) {
      // PHASE 1: INSERT (one document at a time = one commit per doc)
      // Small delay after each commit to allow async auto-optimize to run
      const insertedIds = []
      const insertStart = Date.now()
      for (let i = 0; i < docsPerCycle; i++) {
        const doc = generateDocument()
        const insertResult = await document.insert(agent, { instance: doc })
        const docId = insertResult.body[0].replace(/^.*\//, '')
        insertedIds.push(docId)
        // Allow async auto-optimize to complete (10% chance per commit = ~every 10 commits)
        if (i % 10 === 9) {
          await new Promise(resolve => setTimeout(resolve, 50))
        }
      }
      const insertTime = Date.now() - insertStart

      // Count after INSERT
      const countAfterInsert = await countDocs()

      // PHASE 2: DELETE (one at a time with periodic delay for async optimize)
      const deleteStart = Date.now()
      let deleteIdx = 0
      for (const docId of insertedIds) {
        await document.delete(agent, { body: `TestDoc/${docId}` })
        deleteIdx++
        if (deleteIdx % 10 === 0) {
          await new Promise(resolve => setTimeout(resolve, 50))
        }
      }
      const deleteTime = Date.now() - deleteStart

      // Count after DELETE
      const countAfterDelete = await countDocs()

      // PHASE 3: ROLLUP (flatten layer stack)
      const rollupStart = Date.now()
      let rollupSuccess = false
      try {
        await agent.post(`/api/optimize/${agent.orgName}/${agent.dbName}`)
        rollupSuccess = true
      } catch (e) {
        console.log(`    [WARNING] Rollup failed: ${e.message}`)
      }
      const rollupTime = Date.now() - rollupStart

      if (sampleCycles.includes(cycle)) {
        results.push({
          cycle,
          insertTime,
          deleteTime,
          rollupTime,
          rollupSuccess,
          countAfterInsert,
          countAfterDelete,
          insertPerDoc: (insertTime / docsPerCycle).toFixed(2),
          deletePerDoc: (deleteTime / docsPerCycle).toFixed(2),
        })

        console.log(`  Cycle ${cycle}:`)
        console.log(`    INSERT: ${insertTime}ms (${(insertTime / docsPerCycle).toFixed(2)}ms/doc)`)
        console.log(`    DELETE: ${deleteTime}ms (${(deleteTime / docsPerCycle).toFixed(2)}ms/doc)`)
        console.log(`    ROLLUP: ${rollupTime}ms ${rollupSuccess ? '✓' : '✗'}`)
        console.log(`    Doc count: after INSERT=${countAfterInsert}, after DELETE=${countAfterDelete}`)

        if (countAfterDelete > 0) {
          console.log(`    ⚠️  ${countAfterDelete} documents remain after DELETE!`)
        }
        console.log('')
      }
    }

    console.log('  ===== ACCUMULATION ANALYSIS WITH ROLLUP =====')
    console.log('  Cycle | INSERT | DELETE | ROLLUP | Docs After Insert | Docs After Delete')
    console.log('  ------|--------|--------|--------|-------------------|------------------')
    results.forEach(r => {
      console.log(`  ${String(r.cycle).padStart(5)} | ${String(r.insertTime).padStart(6)}ms | ${String(r.deleteTime).padStart(6)}ms | ${String(r.rollupTime).padStart(6)}ms | ${String(r.countAfterInsert).padStart(17)} | ${String(r.countAfterDelete).padStart(17)}`)
    })

    const firstInsert = results[0].insertTime
    const lastInsert = results[results.length - 1].insertTime
    const insertDegradation = (lastInsert / firstInsert).toFixed(2)

    const firstDelete = results[0].deleteTime
    const lastDelete = results[results.length - 1].deleteTime
    const deleteDegradation = (lastDelete / firstDelete).toFixed(2)

    console.log(`\n  INSERT degradation: ${insertDegradation}x (${firstInsert}ms → ${lastInsert}ms)`)
    console.log(`  DELETE degradation: ${deleteDegradation}x (${firstDelete}ms → ${lastDelete}ms)`)

    // Check if docs are accumulating
    const docsAccumulating = results.some(r => r.countAfterDelete > 0)

    if (docsAccumulating) {
      console.log('\n  🔴 DOCUMENTS ARE ACCUMULATING')
      console.log('  → Deletes are NOT removing documents from database')
      console.log('  → xrdf() must scan growing committed database')
      console.log('  → This explains the O(n²) degradation')

      const avgDocsRemaining = results.reduce((sum, r) => sum + r.countAfterDelete, 0) / results.length
      console.log(`  → Average docs remaining after delete: ${avgDocsRemaining.toFixed(0)}`)
    } else {
      console.log('\n  ✓ Documents are being deleted properly')
      console.log('  → Database is clean after each cycle')
      console.log('  → Problem is NOT accumulated committed documents')
    }

    // Check timing correlation
    const insertTimes = results.map(r => r.insertTime)
    const cycles = results.map(r => r.cycle)

    // Simple linear regression to check if timing grows linearly with cycle count
    const avgCycle = cycles.reduce((a, b) => a + b) / cycles.length
    const avgInsertTime = insertTimes.reduce((a, b) => a + b) / insertTimes.length

    let numerator = 0
    let denominator = 0
    for (let i = 0; i < cycles.length; i++) {
      numerator += (cycles[i] - avgCycle) * (insertTimes[i] - avgInsertTime)
      denominator += (cycles[i] - avgCycle) ** 2
    }
    const slope = numerator / denominator

    console.log(`\n  INSERT timing slope: ${slope.toFixed(2)}ms per cycle`)
    if (slope > 20) {
      console.log('  → Strong linear correlation with cycle count')
      console.log('  → Something accumulates proportionally with cycle count')
    } else if (slope > 5) {
      console.log('  → Moderate correlation with cycle count')
    } else {
      console.log('  → Weak correlation with cycle count')
    }

    console.log('  ====================================\n')

    expect(results.length).to.equal(5)
  })
})
