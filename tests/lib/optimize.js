/**
 * Optimize a complete database including all components
 * Order based on auto-optimize.pl: branch (1), repository (2), database (4)
 * @param {Object} agent - Authenticated agent instance
 * @param {string} path - Database path (e.g., 'admin/testdb')
 * @param {string} branch - Branch name to optimize (e.g., 'main')
 * @returns {Promise} Resolves when optimization completes
 */
async function optimizeDatabase (agent, path, branch) {
  if (!agent || !path) {
    throw new Error('Agent and path are required for database optimization')
  }

  try {
    // Priority 1: Optimize the branch (data product) first
    await optimizeDescriptor(agent, `${path}/local/branch/${branch}`)

    // Priority 2: Optimize the repository (_commits graph)
    await optimizeDescriptor(agent, `${path}/local/_commits`)

    // Priority 4: Optimize the database (_meta) last (slow squash)
    await optimizeDescriptor(agent, `${path}/_meta`)
  } catch (error) {
    console.warn(`Database optimization failed for ${path}:`, error.message)
    // Don't throw - optimization failures shouldn't break tests
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
    const response = await agent.post(optimizePath).send({})

    if (response.status !== 200) {
      console.warn(`Optimization warning: received status ${response.status} for ${path}`)
    }

    if (response.body?.['api:status'] !== 'api:success') {
      console.warn(`Optimization may not have succeeded for ${path}:`, response.body)
    }
  } catch (error) {
    console.warn(`Optimization failed for ${path}:`, error.message)
    // Don't throw - optimization failures shouldn't break tests
  }
}

/**
 * Optimize the system database
 * @param {Object} agent - Authenticated agent instance
 * @returns {Promise} Resolves when optimization completes
 */
async function optimizeSystem (agent) {
  if (!agent) {
    throw new Error('Agent is required for system optimization')
  }

  const optimizePath = '/api/optimize/_system'

  try {
    const response = await agent.post(optimizePath).send({})

    if (response.status !== 200) {
      console.warn(`System optimization warning: received status ${response.status}`)
    }

    if (response.body && response.body['api:status'] === 'api:success') {
      console.log('🔧 System optimized\n')
    } else {
      console.warn('System optimization may not have succeeded:', response.body)
    }
  } catch (error) {
    console.warn('System optimization failed:', error.message)
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

    if (response.body && response.body['api:status'] === 'api:success') {
      console.log(`🔧 Optimized descriptor: ${path}`)
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
  optimizeSystem,
  optimizeRepository,
}
