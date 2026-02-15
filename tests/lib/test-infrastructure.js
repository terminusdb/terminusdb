/**
 * Generic test infrastructure hooks for TerminusDB integration tests
 * Provides standardized before/after optimization and other test utilities
 */

const { optimizeDatabase, optimizeSystem } = require('./optimize.js')

/**
 * Generic before hook for tests that create databases
 * Automatically handles database creation and optimization
 * @param {Object} agent - Authenticated agent instance
 * @param {Object} options - Database creation options
 * @returns {Promise} Resolves when setup is complete
 */
async function databaseTestBefore (agent, options = {}) {
  // Custom before logic can be added here
  await customBeforeLogic(agent, options)

  // Create database (this will automatically optimize via db.js)
  const { db } = require('./index.js')
  await db.create(agent, options)

  // Additional post-creation optimization logic can be added here
  await postCreationLogic(agent, options)
}

/**
 * Generic after hook for tests that create databases
 * @param {Object} agent - Authenticated agent instance
 * @returns {Promise} Resolves when cleanup is complete
 */
async function databaseTestAfter (agent) {
  // Custom cleanup logic can be added here
  await customAfterLogic(agent)

  // Delete database
  const { db } = require('./index.js')
  await db.delete(agent)

  // Additional post-cleanup logic can be added here
  await postCleanupLogic(agent)
}

/**
 * Generic before hook for tests that don't create databases
 * @param {Object} agent - Authenticated agent instance
 * @param {Object} options - Test options
 * @returns {Promise} Resolves when setup is complete
 */
async function systemTestBefore (agent, options = {}) {
  // Custom before logic can be added here
  await customBeforeLogic(agent, options)

  // Optimize system for consistent test performance
  await optimizeSystem(agent)

  // Additional system setup logic can be added here
  await postSystemSetupLogic(agent, options)
}

/**
 * Generic after hook for system-level tests
 * @param {Object} agent - Authenticated agent instance
 * @returns {Promise} Resolves when cleanup is complete
 */
async function systemTestAfter (agent) {
  // Custom cleanup logic can be added here
  await customAfterLogic(agent)

  // Additional system cleanup logic can be added here
  await postSystemCleanupLogic(agent)
}

/**
 * Hook for custom before logic - extend this for additional test setup
 * @param {Object} agent - Authenticated agent instance
 * @param {Object} options - Test options
 */
async function customBeforeLogic (agent, options) {
  // Add custom before logic here
  // This is a placeholder for future test infrastructure enhancements
}

/**
 * Hook for custom after logic - extend this for additional test cleanup
 * @param {Object} agent - Authenticated agent instance
 */
async function customAfterLogic (agent) {
  // Add custom after logic here
  // This is a placeholder for future test infrastructure enhancements
}

/**
 * Hook for post-creation logic - extend this for additional database setup
 * @param {Object} agent - Authenticated agent instance
 * @param {Object} options - Test options
 */
async function postCreationLogic (agent, options) {
  // Add post-creation logic here
  // This is a placeholder for future test infrastructure enhancements
}

/**
 * Hook for post-cleanup logic - extend this for additional database cleanup
 * @param {Object} agent - Authenticated agent instance
 */
async function postCleanupLogic (agent) {
  // Add post-cleanup logic here
  // This is a placeholder for future test infrastructure enhancements
}

/**
 * Hook for post-system setup logic - extend this for additional system setup
 * @param {Object} agent - Authenticated agent instance
 * @param {Object} options - Test options
 */
async function postSystemSetupLogic (agent, options) {
  // Add post-system setup logic here
  // This is a placeholder for future test infrastructure enhancements
}

/**
 * Hook for post-system cleanup logic - extend this for additional system cleanup
 * @param {Object} agent - Authenticated agent instance
 */
async function postSystemCleanupLogic (agent) {
  // Add post-system cleanup logic here
  // This is a placeholder for future test infrastructure enhancements
}

module.exports = {
  databaseTestBefore,
  databaseTestAfter,
  systemTestBefore,
  systemTestAfter,
  // Hooks for extension
  customBeforeLogic,
  customAfterLogic,
  postCreationLogic,
  postCleanupLogic,
  postSystemSetupLogic,
  postSystemCleanupLogic,
}
