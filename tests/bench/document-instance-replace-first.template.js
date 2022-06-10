const { Agent, api, db, document } = require('../lib')

const agent = new Agent().auth()

module.exports = (schema, instance) => {
  let request
  let response
  return {
    beforeEach: async () => {
      // Create the database.
      await db.create(agent)
      // Insert the schema.
      await document.insert(agent, { schema })
      // Insert the instances.
      await document.insert(agent, { instance })
      // Construct the request.
      request = document.replace(agent, { instance }).unverified()
    },

    afterEach: async () => {
      // Delete the database.
      await db.delete(agent)
      // Verify the response.
      api.response.verify(api.response.doc.replaceSuccess)(response)
    },

    fn: async () => {
      // Send the request and get the response.
      response = await request
    },
  }
}
