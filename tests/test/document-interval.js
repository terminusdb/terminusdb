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
        // both dates are inclusive — the stored end is the exclusive
        // day after, and the literal renders the supplied spellings
        interval: '2025-01-01/2025-03-31',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/Q1-2025', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01/2025-03-31')
    })
  })

  describe('literal spelling representations', function () {
    // The same period can be written several ways. Each spelling is its
    // own literal: stored as supplied, read back as supplied.
    const spellings = [
      ['date pair (inclusive ends)', '2025-01-01/2025-03-31'],
      ['zoneless datetime pair (same period)', '2025-01-01T00:00:00/2025-04-01T00:00:00'],
      ['date start, datetime end', '2025-01-01/2025-04-01T00:00:00'],
      ['datetime start, date end', '2025-01-01T00:00:00/2025-04-01'],
      ['duration end with date', 'P3M/2025-03-31'],
      ['date start with duration', '2025-01-01/P3M'],
      ['bare date (whole day)', '2026-10-01'],
      ['bare instant with offset', '2025-03-15T00:00:00+02:00'],
      ['bare instant with Z', '2025-03-15T00:00:00Z'],
      ['bare zoneless instant', '2025-03-15T00:00:00'],
    ]
    spellings.forEach(([label, spelling], i) => {
      it(`round-trips ${label}: ${spelling}`, async function () {
        const name = `rep-${i}`
        await document.insert(agent, { instance: { '@type': 'Event', name, interval: spelling } })
        const r = await document.get(agent, { query: { id: `Event/${name}`, as_list: true } })
        expect(r.body).to.have.lengthOf(1)
        expect(r.body[0].interval).to.equal(spelling)
      })
    })
  })

  describe('bare date roundtrip (as-of instant)', function () {
    it('inserts a single date and reads it back as a single date', async function () {
      const doc = {
        '@type': 'Event',
        name: 'as-of',
        interval: '2025-04-01',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/as-of', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-04-01')
    })
  })

  describe('single instant literals (issue #2626)', function () {
    it('inserts a bare instant with offset and reads it back as the single timestamp', async function () {
      const doc = {
        '@type': 'Event',
        name: 'instant-offset',
        interval: '2025-03-15T00:00:00+02:00',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/instant-offset', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-03-15T00:00:00+02:00')
    })

    it('inserts a bare instant with Z and reads it back as the single timestamp', async function () {
      const doc = {
        '@type': 'Event',
        name: 'instant-z',
        interval: '2025-03-15T00:00:00Z',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/instant-z', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-03-15T00:00:00Z')
    })

    it('inserts a bare zoneless instant and reads it back as the single timestamp', async function () {
      const doc = {
        '@type': 'Event',
        name: 'instant-local',
        interval: '2025-03-15T00:00:00',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/instant-local', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-03-15T00:00:00')
    })

    it('inserts an explicit zero-duration pair and reads it back as a pair', async function () {
      const v = '2025-03-15T00:00:00+02:00/2025-03-15T00:00:00+02:00'
      const doc = {
        '@type': 'Event',
        name: 'zero-pair',
        interval: v,
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/zero-pair', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal(v)
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
      // the supplied spelling is kept verbatim
      expect(r.body[0].interval).to.equal('2025-01-01/P3M')
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
      // the date end is inclusive — stored as the exclusive day after,
      // rendered back as the supplied date
      expect(r.body[0].interval).to.equal('P3M/2025-03-31')
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
      // both dates are inclusive; the literal renders the supplied spellings
      expect(r.body[0].interval).to.equal('2025-04-01/2025-06-30')
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
      // the supplied spelling is kept verbatim
      expect(r.body[0].interval).to.equal('2025-01-01/P1Y')
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
      // the supplied spelling is kept verbatim
      expect(r.body[0].interval).to.equal('2025-01-01/PT1H')
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

  describe('timezone preservation (XBRL OIM)', function () {
    it('preserves positive offset', async function () {
      const doc = {
        '@type': 'Event',
        name: 'tz-pos',
        interval: '2025-01-01T09:00:00+02:00/2025-03-31T18:30:00+02:00',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/tz-pos', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      // Supplied offset round-trips instead of normalizing to Z
      expect(r.body[0].interval).to.equal('2025-01-01T09:00:00+02:00/2025-03-31T18:30:00+02:00')
    })

    it('preserves negative offset', async function () {
      const doc = {
        '@type': 'Event',
        name: 'tz-neg',
        interval: '2025-01-01T09:00:00-05:00/2025-03-31T20:00:00-05:00',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/tz-neg', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01T09:00:00-05:00/2025-03-31T20:00:00-05:00')
    })

    it('preserves zone-less local time without a designator', async function () {
      const doc = {
        '@type': 'Event',
        name: 'tz-none',
        // No zone designator: local time per XBRL OIM, must not render as Z
        interval: '2025-01-01T09:00:00/2025-03-31T18:30:00',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/tz-none', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01T09:00:00/2025-03-31T18:30:00')
    })

    it('preserves mixed offsets on start and end', async function () {
      const doc = {
        '@type': 'Event',
        name: 'tz-mixed',
        interval: '2025-01-01T09:00:00-05:00/2025-03-31T18:30:00+05:30',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/tz-mixed', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01T09:00:00-05:00/2025-03-31T18:30:00+05:30')
    })
  })

  describe('ISO 8601-2 open and unknown endpoints', function () {
    it('inserts and reads back an open end', async function () {
      const doc = {
        '@type': 'Event',
        name: 'open-end',
        interval: '2025-01-01T00:00:00Z/..',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/open-end', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01T00:00:00Z/..')
    })

    it('inserts and reads back an open start', async function () {
      const doc = {
        '@type': 'Event',
        name: 'open-start',
        interval: '../2025-03-31T23:59:59Z',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/open-start', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('../2025-03-31T23:59:59Z')
    })

    it('inserts and reads back the unbounded interval', async function () {
      const doc = {
        '@type': 'Event',
        name: 'forever',
        interval: '../..',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/forever', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('../..')
    })

    it('inserts and reads back an unknown end', async function () {
      const doc = {
        '@type': 'Event',
        name: 'unknown-end',
        interval: '2025-01-01T00:00:00Z/',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/unknown-end', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01T00:00:00Z/')
    })

    it('inserts and reads back both endpoints unknown', async function () {
      const doc = {
        '@type': 'Event',
        name: 'both-unknown',
        interval: '/',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/both-unknown', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('/')
    })

    it('rejects a duration form with an open end', async function () {
      const doc = {
        '@type': 'Event',
        name: 'bad-open-dur',
        interval: 'P1Y/..',
      }
      const r = await document.insert(agent, { instance: doc }).unverified()
      expect(r.status).to.equal(400)
    })
  })

  describe('wide day counts (beyond legacy u8)', function () {
    it('inserts and reads back a duration longer than 255 days', async function () {
      const doc = {
        '@type': 'Event',
        name: 'P300D',
        interval: '2025-01-01/P300D',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/P300D', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01/P300D')
    })

    it('inserts and reads back an explicit interval spanning more than 255 days', async function () {
      const doc = {
        '@type': 'Event',
        name: 'long-span',
        interval: '2025-01-20/2025-10-31',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/long-span', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      // both dates are inclusive; the literal renders the supplied spellings
      expect(r.body[0].interval).to.equal('2025-01-20/2025-10-31')
    })

    it('rejects a malformed interval instead of dropping the field', async function () {
      const doc = {
        '@type': 'Event',
        name: 'bad-interval',
        interval: '2025-01-01/Fnord',
      }
      const r = await document.insert(agent, { instance: doc }).unverified()
      expect(r.status).to.equal(400)
    })
  })

  describe('ISO 8601 week durations', function () {
    // P2W and P14D are equal durations but distinct values — the supplied
    // denomination is part of the literal and must survive end to end.
    it('inserts and reads back a week-denominated duration', async function () {
      const doc = {
        '@type': 'Event',
        name: 'P2W',
        interval: '2025-01-01/P2W',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/P2W', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01/P2W')
    })

    it('keeps a day-denominated equivalent distinct', async function () {
      const doc = {
        '@type': 'Event',
        name: 'P14D',
        interval: '2025-01-01/P14D',
      }
      await document.insert(agent, { instance: doc })

      const r = await document.get(agent, { query: { id: 'Event/P14D', as_list: true } })
      expect(r.body).to.have.lengthOf(1)
      expect(r.body[0].interval).to.equal('2025-01-01/P14D')
    })
  })
})
