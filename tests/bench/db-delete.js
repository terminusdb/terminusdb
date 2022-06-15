const { Agent, api, db, util } = require('../lib')

const agent = new Agent().auth()

let request
let response

module.exports = {
  beforeEach: async () => {
    agent.dbName = util.randomString()
    await db.create(agent)
    request = db.delete(agent).unverified()
  },

  afterEach: async () => {
    api.response.verify(api.response.db.deleteSuccess)(response)
  },

  fn: async () => {
    response = await request
  },
}
