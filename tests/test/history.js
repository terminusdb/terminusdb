const { expect } = require('chai')
const { Agent, util, document } = require('../lib')

describe('history', function () {
  let agent
  let dbName

  before(function () {
    dbName = util.randomString()
    agent = new Agent({ dbName }).auth()
  })

  describe('history with fresh db', function () {
    beforeEach(async function () {
      await agent.post(`/api/db/admin/${dbName}`).send({ label: 'Hello' })
    })

    afterEach(async function () {
      await agent.delete(`/api/db/admin/${dbName}`)
    })

    it('gets a history from changes', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id, a: 'xsd:string' }
      await document.insert(agent, { schema })
      const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'a' }
      await document.insert(agent, { instance: instance1 })

      const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1`, a: 'b' }
      await document.insert(agent, { instance: instance2 })

      const instance1dot0 = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'c' }
      const result3 = await document.replace(agent, { instance: instance1dot0 })
      const version3 = result3.headers['terminusdb-data-version'].split('branch:')[1]

      const historyRequest = await agent.get(`/api/history/admin/${dbName}?id=${id}%2F0`)
      const history = historyRequest.body

      expect(history).to.have.lengthOf(2)
      expect(history[0].author).to.equal('default_author')
      expect(history[0].identifier).to.equal(version3)
    })

    it('finds created and updated date', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id, a: 'xsd:string' }
      await document.insert(agent, { schema })
      const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'a' }
      const result1 = await document.insert(agent, { instance: instance1 })
      const version1 = result1.headers['terminusdb-data-version'].split('branch:')[1]

      const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1`, a: 'b' }
      await document.insert(agent, { instance: instance2 })

      const instance1dot0 = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'c' }
      const result3 = await document.replace(agent, { instance: instance1dot0 })
      const version3 = result3.headers['terminusdb-data-version'].split('branch:')[1]

      const historyRequest = await agent.get(`/api/history/admin/${dbName}?id=${id}%2F0&created=true&updated=true`)
      const history = historyRequest.body

      expect(history.created.identifier).to.equal(version1)
      expect(history.updated.identifier).to.equal(version3)
    })

    it('pages history', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id, a: 'xsd:string' }
      await document.insert(agent, { schema })
      const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'a' }
      const result1 = await document.insert(agent, { instance: instance1 })
      const version1 = result1.headers['terminusdb-data-version'].split('branch:')[1]

      const instance1dot0 = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'b' }
      const result2 = await document.replace(agent, { instance: instance1dot0 })
      const version2 = result2.headers['terminusdb-data-version'].split('branch:')[1]

      const instance1dot1 = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'c' }
      const result3 = await document.replace(agent, { instance: instance1dot1 })
      const version3 = result3.headers['terminusdb-data-version'].split('branch:')[1]

      const instance1dot2 = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'd' }
      const result4 = await document.replace(agent, { instance: instance1dot2 })
      const version4 = result4.headers['terminusdb-data-version'].split('branch:')[1]

      const historyRequest1 = await agent.get(`/api/history/admin/${dbName}?id=${id}%2F0&start=0`)
      const history0toInf = historyRequest1.body
      expect(history0toInf).to.have.lengthOf(4)
      expect(history0toInf[0].identifier).to.equal(version4)

      const historyRequest2 = await agent.get(`/api/history/admin/${dbName}?id=${id}%2F0&start=0&count=1`)
      const history0to1 = historyRequest2.body
      expect(history0to1).to.have.lengthOf(1)

      const historyRequest3 = await agent.get(`/api/history/admin/${dbName}?id=${id}%2F0&start=1&count=2`)
      const history1to3 = historyRequest3.body
      expect(history1to3).to.have.lengthOf(2)
      expect(history1to3[0].identifier).to.equal(version3)
      expect(history1to3[1].identifier).to.equal(version2)

      const historyRequest4 = await agent.get(`/api/history/admin/${dbName}?id=${id}%2F0&start=2&count=2`)
      const history3to4 = historyRequest4.body
      expect(history3to4).to.have.lengthOf(2)
      expect(history3to4[0].identifier).to.equal(version2)
      expect(history3to4[1].identifier).to.equal(version1)
    })
  })
})
