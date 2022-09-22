const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')

describe('backlink inserts', function () {
  let agent

    before(function() {
        agent = new Agent().auth()
    })

  beforeEach(async function () {
      await db.create(agent)
  })

    afterEach(async function () {
        await db.delete(agent)
    })

    it('fails with a subdocument directly inserted with no @linked-by', async function () {
        const schema = {"@type": "Class", "@id": "Subdoc", "@subdocument": [], "@key": {"@type": "Random"}}
        await document.insert(agent, { schema })

        const instance = {"@type": "Subdoc"}
        const r = await document.insert(agent, { instance }).unverified()
        expect(r.status).to.equal(400)
        expect(r.body['api:error']['@type']).to.equal('api:InsertedSubdocumentAsDocument')
    })

    it('fails with a subdocument inserted as part of another document but also having @linked-by', async function () {
        const schema = [
            {"@type": "Class", "@id": "Doc", "subdoc": {"@type":"Set", "@class":"Subdoc"}},
            {"@type": "Class", "@id": "Subdoc", "@subdocument": [], "@key": {"@type": "Random"}}
        ]
        await document.insert(agent, { schema })

        const instance = [{"@type": "Doc", "@capture": "doc1"},
                          {"@type":"Doc", "subdoc": {"@linked-by": {"@ref": "doc1", "@property": "subdoc"}}}]
        const r = await document.insert(agent, { instance }).unverified()
        expect(r.status).to.equal(400)
        expect(r.body['api:error']['@type']).to.equal('api:EmbeddedSubdocumentHasLinkedBy')
    })
})
