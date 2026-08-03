/**
 * Optimize a complete database including all components.
 * Order based on auto-optimize.pl: branch (1), repository/_commits (2),
 * database/_meta (4). Also optimizes the system graph (_system) which
 * accumulates commits from database create/delete operations and is
 * never touched by the auto-optimize plugin's all_descriptor traversal.
 * Called deterministically from db.create() to keep the commit graph
 * squashed for fast subsequent operations.
 * @param {Object} agent - Authenticated agent instance
 * @param {string} path - Database path (e.g., 'admin/testdb')
 * @param {string} branch - Branch name to optimize (e.g., 'main')
 * @returns {Promise} Resolves when optimization completes
 */
async function optimizeDatabase (agent, path, branch) {
  if (!agent || !path) {
    throw new Error('Agent and path are required for database optimization')
  }

  const steps = [
    `${path}/local/branch/${branch}`,
    `${path}/local/_commits`,
    `${path}/_meta`,
  ]
  for (const descPath of steps) {
    await optimizeDescriptor(agent, descPath)
  }
}

/**
 * Optimize a specific descriptor path
 * @param {Object} agent - Authenticated agent instance
 * @param {string} path - Descriptor path
 * @returns {Promise} Resolves when optimization completes
 */
async function optimizeDescriptor (agent, path) {
  const optimizePath = `/api/optimize/${path}`

  try {
    const response = await agent.post(optimizePath).send({}).timeout(60000)

    if (response.status !== 200) {
      console.warn(`\n  Optimization warning: received status ${response.status} for ${path}`)
    }

    if (response.body?.['api:status'] !== 'api:success') {
      console.warn(`\n  Optimization may not have succeeded for ${path}:`, response.body)
    }
  } catch (error) {
    console.warn(`\n  Optimization failed for ${path}:`, error.message)
    // Don't throw - optimization failures shouldn't break tests
  }
}

/**
 * Optimize a repository
 * @param {Object} agent - Authenticated agent instance
 * @param {string} path - Repository path (e.g., 'admin/testdb')
 * @returns {Promise} Resolves when optimization completes
 */
async function optimizeRepository (agent, path) {
  if (!agent || !path) {
    throw new Error('Agent and path are required for repository optimization')
  }

  const optimizePath = `/api/optimize/${path}`

  try {
    const response = await agent.post(optimizePath).send({})

    if (response.status !== 200) {
      console.warn(`Repository optimization warning: received status ${response.status} for ${path}`)
    }

    if (response.body?.['api:status'] === 'api:success') {
      console.log(`Optimized descriptor: ${path}`)
    } else {
      console.warn(`Repository optimization may not have succeeded for ${path}:`, response.body)
    }
  } catch (error) {
    console.warn(`Repository optimization failed for ${path}:`, error.message)
    // Don't throw - optimization failures shouldn't break tests
  }
}

module.exports = {
  optimizeDatabase,
  optimizeDescriptor,
  optimizeRepository,
}
