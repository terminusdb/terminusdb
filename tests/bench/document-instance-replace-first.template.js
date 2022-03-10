const { Agent, db, document, endpoint } = require('../lib')

const agent = new Agent().auth()
const defaults = agent.defaults()

module.exports = (schema, instance) => {
  let request
  let response
  return {
    beforeEach: async () => {
      // Create the database.
      await db.create(agent, endpoint.db(defaults).path).then(db.verifyCreateSuccess)
      const path = endpoint.document(defaults).path
      // Insert the schema.
      await document.insert(agent, path, { schema }).then(document.verifyInsertSuccess)
      // Insert the instances.
      await document.insert(agent, path, { instance }).then(document.verifyInsertSuccess)
      // Construct the request.
      request = document.replace(agent, path, { instance })
    },

    afterEach: async () => {
      // Delete the database.
      await db.del(agent, endpoint.db(defaults).path).then(db.verifyDeleteSuccess)
      // Verify the response.
      document.verifyReplaceSuccess(response)
    },

    fn: async () => {
      // Send the request and get the response.
      response = await request
    },
  }
}
