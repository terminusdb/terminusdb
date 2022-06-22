const { Agent, api, db, document, util } = require('../lib')

const agent = new Agent().auth()

module.exports = (schema) => {
  let request
  let response
  return {
    beforeEach: async () => {
      // Create the database.
      await db.create(agent)
      // Construct the request.
      request = document.insert(agent, { schema }).unverified()
    },

    afterEach: async () => {
      // Delete the database.
      await db.delete(agent)
      // Print the response body for debugging.
      if (util.isNonEmptyObject(response.body)) {
        console.error(JSON.stringify(response.body, null, 2))
      }
      // Verify the response.
      api.response.verify(api.response.doc.insertSuccess)(response)
    },

    fn: async () => {
      // Send the request and get the response.
      response = await request
    },
  }
}
