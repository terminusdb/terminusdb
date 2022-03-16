const { Agent, db, endpoint, util } = require('../lib')

const agent = new Agent().auth()
const { orgName } = agent.defaults()

let dbName
let request
let response

module.exports = {
  beforeEach: async () => {
    dbName = util.randomString()
    request = db.create(agent, endpoint.db({ orgName, dbName }).path)
  },

  afterEach: async () => {
    await db.del(agent, endpoint.db({ orgName, dbName }).path).then(db.verifyDeleteSuccess)
    db.verifyCreateSuccess(response)
  },

  fn: async () => {
    response = await request
  },
}
