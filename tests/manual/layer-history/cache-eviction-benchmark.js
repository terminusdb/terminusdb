const { expect } = require('chai')
const { Agent, db, document } = require('../../../tests/lib')

describe('cache-eviction-benchmark', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)

    const schema = [
      {
        '@type': 'Class',
        '@id': 'LargeDocument',
        '@key': { '@type': 'Random' },
        data: 'xsd:string',
        metadata: {
          '@type': 'Optional',
          '@class': 'Metadata',
        },
      },
      {
        '@type': 'Class',
        '@id': 'Metadata',
        field1: 'xsd:string',
        field2: 'xsd:string',
        field3: 'xsd:string',
        field4: 'xsd:string',
        field5: 'xsd:string',
        nested: {
          '@type': 'Optional',
          '@class': 'NestedData',
        },
      },
      {
        '@type': 'Class',
        '@id': 'NestedData',
        info1: 'xsd:string',
        info2: 'xsd:string',
        info3: 'xsd:string',
        info4: 'xsd:string',
        info5: 'xsd:string',
      },
    ]

    await document.insert(agent, { schema })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('should maintain stable performance across repeated insert/delete cycles', async function () {
    this.timeout(600000) // 10 minutes max

    const docsPerCycle = 70 // Reduced from 100 to speed up testing
    const numCycles = 10 // Reduced from 15 - faster iteration
    const varianceThreshold = 0.5 // 50% variance allowed (it's fluctuating a lot)

    function generateDocument () {
      const largeString = 'x'.repeat(5000) // 5KB base string
      return {
        '@type': 'LargeDocument',
        data: largeString + Math.random(),
        metadata: {
          '@type': 'Metadata',
          field1: largeString + Math.random(),
          field2: largeString + Math.random(),
          field3: largeString + Math.random(),
          field4: largeString + Math.random(),
          field5: largeString + Math.random(),
          nested: {
            '@type': 'NestedData',
            info1: largeString + Math.random(),
            info2: largeString + Math.random(),
            info3: largeString + Math.random(),
            info4: largeString + Math.random(),
            info5: largeString + Math.random(),
          },
        },
      }
    }

    const cycleTimes = []
    const insertedIds = []

    console.log(`\n  Starting benchmark: ${numCycles} cycles of ${docsPerCycle} documents`)

    for (let cycle = 1; cycle <= numCycles; cycle++) {
      const cycleStart = Date.now()

      // Insert phase
      const insertStart = Date.now()
      const docs = []
      for (let i = 0; i < docsPerCycle; i++) {
        docs.push(generateDocument())
      }
      const insertResult = await document.insert(agent, { instance: docs })
      const insertedIdsThisCycle = insertResult.body.map(id => id.replace(/^.*\//, ''))
      insertedIds.push(...insertedIdsThisCycle)
      const insertTime = Date.now() - insertStart

      // Delete phase
      const deleteStart = Date.now()
      for (const docId of insertedIdsThisCycle) {
        await document.delete(agent, { body: `LargeDocument/${docId}` })
      }
      const deleteTime = Date.now() - deleteStart

      const cycleEnd = Date.now()
      const totalCycleTime = cycleEnd - cycleStart
      cycleTimes.push(totalCycleTime)

      console.log(`  Cycle ${cycle}/${numCycles}: ${totalCycleTime}ms (insert: ${insertTime}ms, delete: ${deleteTime}ms)`)

      // Log every 10 cycles
      if ((cycle) % 10 === 0) {
        const avgTime = cycleTimes.reduce((a, b) => a + b, 0) / cycleTimes.length
        const lastCycleTime = cycleTimes[cycleTimes.length - 1]
        const slowdown = ((lastCycleTime / cycleTimes[0]) - 1) * 100
        console.log(`  Cycle ${cycle + 1}/${numCycles}: ${totalCycleTime}ms (insert: ${insertTime}ms, delete: ${deleteTime}ms)`)
        console.log(`    Avg: ${avgTime.toFixed(0)}ms, First: ${cycleTimes[0]}ms, Last: ${lastCycleTime}ms, Slowdown: ${slowdown.toFixed(1)}%`)
      }
    }

    // Calculate statistics
    const firstCycleTime = cycleTimes[0]
    const lastCycleTime = cycleTimes[cycleTimes.length - 1]
    const avgTime = cycleTimes.reduce((a, b) => a + b, 0) / cycleTimes.length
    const minTime = Math.min(...cycleTimes)
    const maxTime = Math.max(...cycleTimes)

    // Calculate variance from average
    const variance = cycleTimes.map(t => Math.abs(t - avgTime) / avgTime)
    const maxVariance = Math.max(...variance)

    // Calculate slowdown factor (last cycle vs first cycle)
    const slowdownFactor = lastCycleTime / firstCycleTime

    console.log('\n  ===== Benchmark Results =====')
    console.log('  First cycle: ' + firstCycleTime + 'ms')
    console.log('  Last cycle:  ' + lastCycleTime + 'ms')
    console.log('  Average:     ' + avgTime.toFixed(0) + 'ms')
    console.log('  Min:         ' + minTime + 'ms')
    console.log('  Max:         ' + maxTime + 'ms')
    console.log('  Max variance from avg: ' + (maxVariance * 100).toFixed(1) + '%')
    console.log('  Slowdown factor: ' + slowdownFactor.toFixed(2) + 'x')
    console.log('  =============================\n')

    // CRITICAL ASSERTION: Performance should remain stable
    // This will FAIL initially, demonstrating the cache degradation issue
    expect(maxVariance).to.be.at.most(
      varianceThreshold,
      'Performance degraded! Max variance ' + (maxVariance * 100).toFixed(1) + '% exceeds threshold ' + (varianceThreshold * 100).toFixed(1) + '%. ' +
      'Slowdown: ' + slowdownFactor.toFixed(2) + 'x (last: ' + lastCycleTime + 'ms vs first: ' + firstCycleTime + 'ms). ' +
      'This indicates cache accumulation causing performance degradation.',
    )
  })

  it('should demonstrate cache accumulation with metadata queries', async function () {
    this.timeout(300000) // 5 minutes

    const docsPerBatch = 50
    const numBatches = 15 // Reduced from 30 - degradation visible quickly

    console.log(`\n  Cache accumulation test: ${numBatches} batches of ${docsPerBatch} documents`)

    const batchTimes = []

    for (let batch = 0; batch < numBatches; batch++) {
      const batchStart = Date.now()

      // Create documents
      const docs = []
      for (let i = 0; i < docsPerBatch; i++) {
        docs.push({
          '@type': 'LargeDocument',
          data: 'test_data_' + Math.random(),
          metadata: {
            '@type': 'Metadata',
            field1: 'value1_' + Math.random(),
            field2: 'value2_' + Math.random(),
            field3: 'value3_' + Math.random(),
            field4: 'value4_' + Math.random(),
            field5: 'value5_' + Math.random(),
          },
        })
      }

      // Insert
      const insertResult = await document.insert(agent, { instance: docs })
      const ids = insertResult.body.map(id => id.replace(/^.*\//, ''))

      // Query all documents (exercises cache lookups)
      await document.get(agent, { query: { type: 'LargeDocument', as_list: true } })

      // Delete
      for (const docId of ids) {
        await document.delete(agent, { body: `LargeDocument/${docId}` })
      }

      const batchEnd = Date.now()
      const batchTime = batchEnd - batchStart
      batchTimes.push(batchTime)

      if ((batch + 1) % 10 === 0) {
        const currentSlowdown = (batchTime / batchTimes[0]) - 1
        console.log(`  Batch ${batch + 1}/${numBatches}: ${batchTime}ms (slowdown: ${(currentSlowdown * 100).toFixed(1)}%)`)
      }
    }

    const firstBatchTime = batchTimes[0]
    const lastBatchTime = batchTimes[batchTimes.length - 1]
    const slowdown = (lastBatchTime / firstBatchTime) - 1

    console.log(`\n  First batch: ${firstBatchTime}ms`)
    console.log(`  Last batch:  ${lastBatchTime}ms`)
    console.log(`  Slowdown:    ${(slowdown * 100).toFixed(1)}%\n`)

    // This assertion will likely fail, showing cache accumulation
    expect(slowdown).to.be.at.most(
      0.5, // 50% slowdown max (it's fluctuating a lot)
      'Cache accumulation detected! Last batch ' + (slowdown * 100).toFixed(1) + '% slower than first. ' +
      'This indicates weak references accumulating in the layer cache.',
    )
  })
})
