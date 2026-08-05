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
        // end date is always inclusive if just a date
        // uses convention from XBRL JSON
        interval: '2025-01-01/2025-03-31',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/Q1-2025', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      // Date-only start normalizes to T00:00:00Z; date-only end bumps to next day (exclusive)
      expect(r.body[0].interval).to.equal('2025-01-01T00:00:00Z/2025-04-01T00:00:00Z')
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
      // Date-only start normalizes to T00:00:00Z; duration preserved
      expect(r.body[0].interval).to.equal('2025-01-01T00:00:00Z/P3M')
    })
  })

  describe('duration+end roundtrip', function () {
    it('inserts and reads back duration+end interval', async function () {
      const doc = {
        '@type': 'Event',
        name: 'Q1-durend',
        interval: 'P3M/2025-03-31',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/Q1-durend', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      // Date-only end bumps to start of next day (exclusive); duration preserved
      expect(r.body[0].interval).to.equal('P3M/2025-04-01T00:00:00Z')
    })
  })

  describe('update roundtrip', function () {
    it('updates interval and reads back new value', async function () {
      const doc = {
        '@type': 'Event',
        name: 'updatable',
        interval: '2025-01-01/2025-03-31',
      }
      await document.insert(agent, { instance: doc })

      const updated = {
        '@type': 'Event',
        name: 'updatable',
        interval: '2025-04-01/2025-06-30',
      }
      await document.replace(agent, { instance: updated })

      const r = await document.get(agent, { query: { id: 'Event/updatable', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      // Date-only start normalizes to T00:00:00Z; date-only end bumps to next day (exclusive)
      expect(r.body[0].interval).to.equal('2025-04-01T00:00:00Z/2025-07-01T00:00:00Z')
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
      // Date-only start normalizes to T00:00:00Z; duration preserved
      expect(r.body[0].interval).to.equal('2025-01-01T00:00:00Z/P1Y')
    })
  })

  describe('datetime interval with time components', function () {
    it('preserves datetime with time parts', async function () {
      const doc = {
        '@type': 'Event',
        name: 'with-time',
        interval: '2025-01-01T10:30:00Z/2025-03-31T15:45:00Z',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/with-time', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      // Fully-qualified datetimes are used as-is (normalized to UTC)
      expect(r.body[0].interval).to.equal('2025-01-01T10:30:00Z/2025-03-31T15:45:00Z')
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
      // Date-only start normalizes to T00:00:00Z; duration preserved
      expect(r.body[0].interval).to.equal('2025-01-01T00:00:00Z/PT1H')
    })

    it('preserves nanosecond precision in datetime and duration', async function () {
      const doc = {
        '@type': 'Event',
        name: 'nano-dur',
        interval: '2025-01-01T10:30:00.123456789Z/PT0.123456789S',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/nano-dur', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      // Nanosecond fractional seconds preserved on both datetime and duration
      expect(r.body[0].interval).to.equal('2025-01-01T10:30:00.123456789Z/PT0.123456789S')
    })
  })

  describe('timezone offset normalization to UTC', function () {
    it('normalizes positive offset to UTC', async function () {
      const doc = {
        '@type': 'Event',
        name: 'tz-pos',
        // +02:00 means local time is 2 hours ahead of UTC, so 09:00 local = 07:00 UTC
        interval: '2025-01-01T09:00:00+02:00/2025-03-31T18:30:00+02:00',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/tz-pos', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      // Both endpoints normalized to UTC: +02:00 subtracted from local time
      expect(r.body[0].interval).to.equal('2025-01-01T07:00:00Z/2025-03-31T16:30:00Z')
    })

    it('normalizes negative offset to UTC', async function () {
      const doc = {
        '@type': 'Event',
        name: 'tz-neg',
        // -05:00 means local time is 5 hours behind UTC, so 09:00 local = 14:00 UTC
        interval: '2025-01-01T09:00:00-05:00/2025-03-31T20:00:00-05:00',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/tz-neg', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      // Both endpoints normalized to UTC: -05:00 added to local time
      expect(r.body[0].interval).to.equal('2025-01-01T14:00:00Z/2025-04-01T01:00:00Z')
    })
  })
})
