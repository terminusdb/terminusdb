const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('document-interval', function () {
  let agent

  const schema = {
    '@id': 'Event',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    interval: 'xdd:dateTimeInterval',
  }

  const schemaOptional = {
    '@id': 'MaybeEvent',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    interval: {
      '@type': 'Optional',
      '@class': 'xdd:dateTimeInterval',
    },
  }

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
    await document.insert(agent, { schema: [schema, schemaOptional] })
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('explicit start/end roundtrip', function () {
    it('inserts and reads back explicit date interval', async function () {
      const doc = {
        '@type': 'Event',
        name: 'Q1-2025',
        interval: '2025-01-01/2025-04-01',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/Q1-2025', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01/2025-04-01')
    })
  })

  describe('start+duration roundtrip', function () {
    it('inserts and reads back start+duration interval', async function () {
      const doc = {
        '@type': 'Event',
        name: 'Q1-duration',
        interval: '2025-01-01/P3M',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/Q1-duration', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01/P3M')
    })
  })

  describe('duration+end roundtrip', function () {
    it('inserts and reads back duration+end interval', async function () {
      const doc = {
        '@type': 'Event',
        name: 'Q1-durend',
        interval: 'P3M/2025-04-01',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/Q1-durend', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('P3M/2025-04-01')
    })
  })

  describe('update roundtrip', function () {
    it('updates interval and reads back new value', async function () {
      const doc = {
        '@type': 'Event',
        name: 'updatable',
        interval: '2025-01-01/2025-02-01',
      }
      await document.insert(agent, { instance: doc })

      const updated = {
        '@type': 'Event',
        name: 'updatable',
        interval: '2025-06-01/2025-09-01',
      }
      await document.replace(agent, { instance: updated })

      const r = await document.get(agent, { query: { id: 'Event/updatable', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-06-01/2025-09-01')
    })
  })

  describe('optional interval', function () {
    it('inserts with null optional interval', async function () {
      const doc = {
        '@type': 'MaybeEvent',
        name: 'no-interval',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'MaybeEvent/no-interval', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.not.exist
    })

    it('inserts with non-null optional interval', async function () {
      const doc = {
        '@type': 'MaybeEvent',
        name: 'has-interval',
        interval: '2025-01-01/P1Y',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'MaybeEvent/has-interval', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01/P1Y')
    })
  })

  describe('datetime interval with time components', function () {
    it('preserves datetime with time parts', async function () {
      const doc = {
        '@type': 'Event',
        name: 'with-time',
        interval: '2025-01-01T10:30:00Z/2025-04-01T15:45:00Z',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/with-time', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01T10:30:00Z/2025-04-01T15:45:00Z')
    })
  })

  describe('duration with time parts', function () {
    it('preserves duration with hours', async function () {
      const doc = {
        '@type': 'Event',
        name: 'hour-dur',
        interval: '2025-01-01/PT1H',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/hour-dur', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01/PT1H')
    })
  })
})
