const { expect } = require('chai')
const { Agent, db, util } = require('../lib')

describe('prefix-management', function () {
  this.timeout(10000)

  let agent
  before(async function () {
    agent = new Agent().auth()
  })

  beforeEach(async function () {
    agent.dbName = util.randomString()
    await db.create(agent)
  })

  afterEach(async function () {
    await db.delete(agent)
  })

  describe('GET /api/prefix', function () {
    it('retrieves an existing prefix', async function () {
      // First add a prefix
      const addRes = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/myprefix`)
        .send({ uri: 'http://example.org/myprefix/' })
      expect(addRes.status).to.equal(201)

      // Then retrieve it
      const getRes = await agent.get(`/api/prefix/${agent.orgName}/${agent.dbName}/myprefix`)
      expect(getRes.status).to.equal(200)
      expect(getRes.body['api:prefix_name']).to.equal('myprefix')
      expect(getRes.body['api:prefix_uri']).to.equal('http://example.org/myprefix/')
    })

    it('returns 404 for non-existent prefix', async function () {
      const res = await agent.get(`/api/prefix/${agent.orgName}/${agent.dbName}/nonexistent`)
      expect(res.status).to.equal(404)
    })
  })

  describe('POST /api/prefix (add)', function () {
    it('adds a new prefix', async function () {
      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/newprefix`)
        .send({ uri: 'http://example.org/new/' })
      expect(res.status).to.equal(201)
      expect(res.body['api:prefix_name']).to.equal('newprefix')
      expect(res.body['api:prefix_uri']).to.equal('http://example.org/new/')
      expect(res.body['api:status']).to.equal('api:success')
    })

    it('fails when adding duplicate prefix with different IRI', async function () {
      await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/dupprefix`)
        .send({ uri: 'http://example.org/first/' })

      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/dupprefix`)
        .send({ uri: 'http://example.org/second/' })
      expect(res.status).to.equal(400)
      expect(res.body['api:error']['@type']).to.equal('api:PrefixAlreadyExists')
    })

    it('returns 204 when adding prefix with same name and same IRI (idempotent)', async function () {
      const prefixUri = 'http://example.org/idempotent/'

      // First creation should return 201
      const firstRes = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/idempotent`)
        .send({ uri: prefixUri })
      expect(firstRes.status).to.equal(201)

      // Second creation with same name AND same IRI should return 204 (no-op)
      const secondRes = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/idempotent`)
        .send({ uri: prefixUri })
      expect(secondRes.status).to.equal(204)
    })

    it('fails when adding prefix with IRI already used by another prefix', async function () {
      const sharedUri = 'http://example.org/shared/'

      // First prefix with this IRI
      const firstRes = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/first_prefix`)
        .send({ uri: sharedUri })
      expect(firstRes.status).to.equal(201)

      // Second prefix with different name but same IRI should fail
      const secondRes = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/second_prefix`)
        .send({ uri: sharedUri })
      expect(secondRes.status).to.equal(400)
      expect(secondRes.body['api:error']['@type']).to.equal('api:PrefixUriAlreadyInUse')
    })

    it('adds prefix with emoji in IRI', async function () {
      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/emoji`)
        .send({ uri: 'http://example.org/🚀/' })
      expect(res.status).to.equal(201)
      expect(res.body['api:prefix_uri']).to.equal('http://example.org/🚀/')
    })

    it('fails with invalid IRI (no scheme)', async function () {
      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/badiri`)
        .send({ uri: 'not a valid iri' })
      expect(res.status).to.equal(400)
    })

    it('fails for reserved prefix names starting with @', async function () {
      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/@custom`)
        .send({ uri: 'http://example.org/custom/' })
      expect(res.status).to.equal(400)
    })

    it('adds prefix with dots (no URL encoding needed)', async function () {
      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/v1.0`)
        .send({ uri: 'http://example.org/v1/' })
      expect(res.status).to.equal(201)
      expect(res.body['api:prefix_name']).to.equal('v1.0')
    })

    it('adds prefix with hyphens and underscores', async function () {
      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/my-api_v2`)
        .send({ uri: 'http://api.example.org/v2/' })
      expect(res.status).to.equal(201)
      expect(res.body['api:prefix_name']).to.equal('my-api_v2')
    })

    it('adds prefix starting with underscore (NCName allows this)', async function () {
      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/_internal`)
        .send({ uri: 'http://example.org/internal/' })
      expect(res.status).to.equal(201)
      expect(res.body['api:prefix_name']).to.equal('_internal')
    })

    it('adds prefix ending with dot (NCName allows this)', async function () {
      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/v1.`)
        .send({ uri: 'http://example.org/v1/' })
      expect(res.status).to.equal(201)
      expect(res.body['api:prefix_name']).to.equal('v1.')
    })

    it('fails with invalid prefix name starting with digit', async function () {
      const res = await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/1invalid`)
        .send({ uri: 'http://example.org/' })
      expect(res.status).to.equal(400)
      expect(res.body['api:error']['@type']).to.equal('api:ReservedPrefix')
    })
  })

  describe('PUT /api/prefix (update)', function () {
    it('updates an existing prefix', async function () {
      // First add a prefix
      await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/updateme`)
        .send({ uri: 'http://example.org/old/' })

      // Then update it
      const res = await agent.put(`/api/prefix/${agent.orgName}/${agent.dbName}/updateme`)
        .send({ uri: 'http://example.org/new/' })
      expect(res.status).to.equal(200)
      expect(res.body['api:prefix_uri']).to.equal('http://example.org/new/')
    })

    it('fails when updating non-existent prefix', async function () {
      const res = await agent.put(`/api/prefix/${agent.orgName}/${agent.dbName}/nonexistent`)
        .send({ uri: 'http://example.org/new/' })
      expect(res.status).to.equal(404)
    })

    it('updates prefix with emoji in new IRI', async function () {
      await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/emojipdate`)
        .send({ uri: 'http://example.org/old/' })

      const res = await agent.put(`/api/prefix/${agent.orgName}/${agent.dbName}/emojipdate`)
        .send({ uri: 'http://example.org/🎉/updated/' })
      expect(res.status).to.equal(200)
      expect(res.body['api:prefix_uri']).to.equal('http://example.org/🎉/updated/')
    })
  })

  describe('PUT /api/prefix?create=true (upsert)', function () {
    it('creates prefix if it does not exist', async function () {
      const res = await agent.put(`/api/prefix/${agent.orgName}/${agent.dbName}/upsertme?create=true`)
        .send({ uri: 'http://example.org/upserted/' })
      expect(res.status).to.equal(200)
      expect(res.body['api:prefix_uri']).to.equal('http://example.org/upserted/')
    })

    it('updates prefix if it already exists', async function () {
      // First add a prefix
      await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/upsertupdate`)
        .send({ uri: 'http://example.org/initial/' })

      // Then upsert it
      const res = await agent.put(`/api/prefix/${agent.orgName}/${agent.dbName}/upsertupdate?create=true`)
        .send({ uri: 'http://example.org/upserted/' })
      expect(res.status).to.equal(200)
      expect(res.body['api:prefix_uri']).to.equal('http://example.org/upserted/')

      // Verify it was updated
      const getRes = await agent.get(`/api/prefix/${agent.orgName}/${agent.dbName}/upsertupdate`)
      expect(getRes.body['api:prefix_uri']).to.equal('http://example.org/upserted/')
    })
  })

  describe('DELETE /api/prefix', function () {
    it('deletes an existing prefix', async function () {
      // First add a prefix
      await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/deleteme`)
        .send({ uri: 'http://example.org/delete/' })

      // Then delete it
      const res = await agent.delete(`/api/prefix/${agent.orgName}/${agent.dbName}/deleteme`)
      expect(res.status).to.equal(200)
      expect(res.body['api:status']).to.equal('api:success')

      // Verify it's gone
      const getRes = await agent.get(`/api/prefix/${agent.orgName}/${agent.dbName}/deleteme`)
      expect(getRes.status).to.equal(404)
    })

    it('fails when deleting non-existent prefix', async function () {
      const res = await agent.delete(`/api/prefix/${agent.orgName}/${agent.dbName}/nonexistent`)
      expect(res.status).to.equal(404)
    })

    it('fails when deleting reserved prefix', async function () {
      const res = await agent.delete(`/api/prefix/${agent.orgName}/${agent.dbName}/@base`)
      expect(res.status).to.equal(400)
    })
  })

  describe('prefix retrieval via GET /api/prefixes', function () {
    it('shows added prefix in all prefixes', async function () {
      // Add a prefix
      await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/visible`)
        .send({ uri: 'http://example.org/visible/' })

      // Get all prefixes
      const res = await agent.get(`/api/prefixes/${agent.orgName}/${agent.dbName}`)
      expect(res.status).to.equal(200)
      expect(res.body.visible).to.equal('http://example.org/visible/')
    })
  })

  describe('authorization', function () {
    it('rejects unauthorized POST with 403', async function () {
      const unauthAgent = new Agent()
      const res = await unauthAgent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/testprefix`)
        .send({ uri: 'http://example.org/test/' })
      expect(res.status).to.equal(403)
    })

    it('rejects unauthorized DELETE with 403', async function () {
      // First add with authenticated agent
      await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/authtest`)
        .send({ uri: 'http://example.org/authtest/' })

      // Try to delete without auth
      const unauthAgent = new Agent()
      const res = await unauthAgent.delete(`/api/prefix/${agent.orgName}/${agent.dbName}/authtest`)
      expect(res.status).to.equal(403)
    })

    it('rejects unauthorized GET individual prefix with 403', async function () {
      // First add with authenticated agent
      await agent.post(`/api/prefix/${agent.orgName}/${agent.dbName}/getauthtest`)
        .send({ uri: 'http://example.org/getauthtest/' })

      // Try to get without auth
      const unauthAgent = new Agent()
      const res = await unauthAgent.get(`/api/prefix/${agent.orgName}/${agent.dbName}/getauthtest`)
      expect(res.status).to.equal(403)
    })
  })
})
