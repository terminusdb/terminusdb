const { Agent, db, endpoint, util } = require('../lib')

const agent = new Agent().auth()
const { orgName } = agent.defaults()

let dbName
let request
let response

module.exports = {
  beforeEach: async () => {
    dbName = util.randomString()
    await db.create(agent, endpoint.db({ orgName, dbName }).path).then(db.verifyCreateSuccess)
    request = db.del(agent, endpoint.db({ orgName, dbName }).path)
  },

  afterEach: async () => {
    db.verifyDeleteSuccess(response)
  },

  fn: async () => {
    response = await request
  },
}
