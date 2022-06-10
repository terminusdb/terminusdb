const { Agent, api, db, util } = require('../lib')

const agent = new Agent().auth()

let request
let response

module.exports = {
  beforeEach: async () => {
    agent.dbName = util.randomString()
    request = db.create(agent).unverified()
  },

  afterEach: async () => {
    await db.delete(agent)
    api.response.verify(api.response.db.createSuccess)(response)
  },

  fn: async () => {
    response = await request
  },
}
