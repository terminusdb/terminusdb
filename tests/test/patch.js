const { expect } = require('chai')
const { Agent, api, document, db, util } = require('../lib')

describe('patch', function () {
  let agent
  let ty1
  let ids
  let id1
  let id2

  before(function () {
    agent = new Agent().auth()
  })

  describe('1 database, shared', function () {
    beforeEach(async function () {
      await db.create(agent)
      ty1 = util.randomString()
      const schema = [
        {
          '@type': 'Class',
          '@id': ty1,
          name: 'xsd:string',
        },
      ]
      const instance = [
        {
          '@type': ty1,
          name: 'foo',
        },
        {
          '@type': ty1,
          name: 'bar',
        },
      ]
      await document.insert(agent, { schema })
      const response = await document.insert(agent, { instance })
      ids = response.body
      id1 = ids[0]
      id2 = ids[1]
    })

    afterEach(async function () {
      await db.delete(agent)
    })

    it('applies patch to db', async function () {
      const path = api.path.patchDb(agent)
      const patch = { '@id': id1, name: { '@op': 'SwapValue', '@before': 'foo', '@after': 'bar' } }
      const author = 'me'
      const message = 'yo'
      const res = await agent.post(path).send({ patch, author, message })
      expect(res.body).to.deep.equal([id1])
    })

    it('applies patch to db and gets a conflict', async function () {
      const path = api.path.patchDb(agent)
      const patch = { '@id': id1, name: { '@op': 'SwapValue', '@before': 'quux', '@after': 'zippo' } }
      const author = 'me'
      const message = 'yo'
      const res = await agent.post(path).send({ patch, author, message })

      // Verify request_id exists and is valid UUID v4
      expect(res.body).to.have.property('api:request_id')
      expect(res.body['api:request_id']).to.match(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i)

      // Remove request_id for comparison of the rest of the structure
      const { 'api:request_id': _, ...bodyWithoutRequestId } = res.body

      expect(bodyWithoutRequestId).to.deep.equal({
        '@type': 'api:PatchResponse',
        'api:error': {
          '@type': 'api:PatchConflict',
          'api:conflicts': [
            {
              '@id': id1,
              name: {
                '@expected': 'quux',
                '@found': 'foo',
                '@op': 'Conflict',
              },
            },
          ],
        },
        'api:message': 'The patch did not apply cleanly because of the attached conflicts',
        'api:status': 'api:conflict',
      })
    })

    it('applies several patches to db with final state match', async function () {
      const path = api.path.patchDb(agent)
      const patch = [{ '@id': id1, name: { '@op': 'SwapValue', '@before': 'foo', '@after': 'bar' } },
        { '@id': id2, name: { '@op': 'SwapValue', '@before': 'foo', '@after': 'bar' } }]
      const author = 'me'
      const message = 'yo'
      const res = await agent.post(path).send({ patch, author, message })
      expect(res.body).to.deep.equal([id1, id2])
    })

    it('fails apply several patches to db without final state match', async function () {
      const path = api.path.patchDb(agent)
      const patch = [{ '@id': id1, name: { '@op': 'SwapValue', '@before': 'foo', '@after': 'bar' } },
        { '@id': id2, name: { '@op': 'SwapValue', '@before': 'foo', '@after': 'bar' } }]
      const author = 'me'
      const message = 'yo'
      const res = await agent.post(path).send({
        match_final_state: false,
        patch,
        author,
        message,
      })

      // Verify request_id exists and is valid UUID v4
      expect(res.body).to.have.property('api:request_id')
      expect(res.body['api:request_id']).to.match(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i)

      // Remove request_id for comparison of the rest of the structure
      const { 'api:request_id': _, ...bodyWithoutRequestId } = res.body

      expect(bodyWithoutRequestId).to.deep.equal({
        '@type': 'api:PatchResponse',
        'api:error':
        {
          '@type': 'api:PatchConflict',
          'api:conflicts': [
            {
              '@id': id2,
              name: { '@expected': 'foo', '@found': 'bar', '@op': 'Conflict' },
            },
          ],
        },
        'api:message': 'The patch did not apply cleanly because of the attached conflicts',
        'api:status': 'api:conflict',
      })
    })
  })
})
