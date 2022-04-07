const { Agent, db, document, endpoint, util } = require('../lib')

const agent = new Agent().auth()
const defaults = agent.defaults()

module.exports = (schema) => {
  let request
  let response
  return {
    beforeEach: async () => {
      // Create the database.
      await db.create(agent, endpoint.db(defaults).path).then(db.verifyCreateSuccess)
      // Construct the request.
      request = document.insert(agent, endpoint.document(defaults).path, { schema })
    },

    afterEach: async () => {
      // Delete the database.
      await db.del(agent, endpoint.db(defaults).path).then(db.verifyDeleteSuccess)
      // Print the response body for debugging.
      if (util.isNonEmptyObject(response.body)) {
        console.error(JSON.stringify(response.body, null, 2))
      }
      // Verify the response.
      document.verifyInsertSuccess(response)
    },

    fn: async () => {
      // Send the request and get the response.
      response = await request
    },
  }
}
