const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

function fieldValuePair (field, value) {
  return { '@type': 'FieldValuePair', field, value }
}

function value (data) {
  return { '@type': 'Value', data }
}

function dictTemplate (pairs) {
  return {
    '@type': 'Value',
    dictionary: { '@type': 'DictionaryTemplate', data: pairs },
  }
}

function insertDocument (docType, name, payloadValue) {
  return {
    '@type': 'InsertDocument',
    document: dictTemplate([
      fieldValuePair('@type', value({ '@type': 'xsd:string', '@value': docType })),
      fieldValuePair('name', value({ '@type': 'xsd:string', '@value': name })),
      fieldValuePair('payload', payloadValue),
    ]),
  }
}

function payloadDoc (pairs) {
  return dictTemplate(pairs.map(([field, data]) => (
    fieldValuePair(field, data['@type'] === 'Value' ? data : value(data))
  )))
}

describe('sys:JSON via WOQL', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent, { label: 'Test sys:JSON via WOQL', schema: true })

    await document.insert(agent, {
      schema: [
        {
          '@type': 'Class',
          '@id': 'Doc',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          payload: 'sys:JSON',
        },
        {
          '@type': 'Class',
          '@id': 'FlagDoc',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          flag: 'xsd:boolean',
        },
      ],
    })
  })

  after(async function () {
    await db.delete(agent)
  })

  async function insertAndRead (name, payloadValue, expected) {
    const query = insertDocument('Doc', name, payloadValue)
    const insertResult = await woql.post(agent, query).unverified()

    if (insertResult.status !== 200) {
      throw new Error(`WOQL InsertDocument failed: ${JSON.stringify(insertResult.body)}`)
    }
    expect(insertResult.body.inserts).to.be.greaterThan(0)

    const docResult = await document.get(agent, {
      query: { id: `Doc/${name}`, as_list: true },
    })
    expect(docResult.body).to.be.an('array').with.lengthOf(1)
    expect(docResult.body[0].payload).to.deep.equal(expected)
  }

  describe('InsertDocument scalar JSON values', function () {
    const cases = [
      ['string', { '@type': 'xsd:string', '@value': 'hello' }, { v: 'hello' }],
      ['empty-string', { '@type': 'xsd:string', '@value': '' }, { v: '' }],
      ['integer', { '@type': 'xsd:integer', '@value': 42 }, { v: 42 }],
      ['decimal', { '@type': 'xsd:decimal', '@value': 3.5 }, { v: 3.5 }],
      ['double', { '@type': 'xsd:double', '@value': 1.5e300 }, { v: 1.5e300 }],
      ['boolean-true', { '@type': 'xsd:boolean', '@value': true }, { v: true }],
      ['boolean-false', { '@type': 'xsd:boolean', '@value': false }, { v: false }],
      ['untyped-true', { '@value': true }, { v: true }],
    ]

    for (const [name, data, expected] of cases) {
      it(`inserts and round-trips ${name}`, async function () {
        await insertAndRead(name, payloadDoc([['v', data]]), expected)
      })
    }

    it('inserts a boolean beside a string in the same object', async function () {
      await insertAndRead(
        'bool-beside-string',
        payloadDoc([
          ['v', { '@type': 'xsd:string', '@value': 's' }],
          ['flag', { '@type': 'xsd:boolean', '@value': true }],
        ]),
        { v: 's', flag: true },
      )
    })

    it('inserts a nested object holding a boolean', async function () {
      await insertAndRead(
        'nested-boolean',
        payloadDoc([
          ['v', dictTemplate([
            fieldValuePair('flag', value({ '@type': 'xsd:boolean', '@value': true })),
          ])],
        ]),
        { v: { flag: true } },
      )
    })
  })

  describe('InsertDocument scalars inside sys:JSON lists', function () {
    const cases = [
      ['list-string', [{ '@type': 'xsd:string', '@value': 's' }], ['s']],
      ['list-integer', [{ '@type': 'xsd:integer', '@value': 3 }], [3]],
      ['list-boolean', [{ '@type': 'xsd:boolean', '@value': true }], [true]],
      ['list-empty', [], []],
      ['list-objects', [{ k: 'v' }], [{ k: 'v' }]],
    ]

    for (const [name, listData, expected] of cases) {
      it(`inserts and round-trips ${name}`, async function () {
        const items = listData.map((item) => {
          if (item['@type'] === 'Value') { return item }
          if (item['@type']) { return value(item) }
          return dictTemplate(Object.entries(item).map(([f, d]) => fieldValuePair(f, value(d))))
        })
        await insertAndRead(
          name,
          dictTemplate([fieldValuePair('v', { '@type': 'Value', list: items })]),
          { v: expected },
        )
      })
    }

    it('inserts a nested list holding a scalar', async function () {
      await insertAndRead(
        'list-nested-scalar',
        dictTemplate([
          fieldValuePair('v', {
            '@type': 'Value',
            list: [{ '@type': 'Value', list: [value({ '@type': 'xsd:string', '@value': 's' })] }],
          }),
        ]),
        { v: [['s']] },
      )
    })
  })

  describe('UpdateDocument with JSON scalars', function () {
    it('updates a sys:JSON payload to hold a boolean', async function () {
      await insertAndRead('update-target', payloadDoc([['v', { '@type': 'xsd:string', '@value': 'before' }]]), { v: 'before' })

      const update = {
        '@type': 'UpdateDocument',
        identifier: { '@type': 'NodeValue', node: 'Doc/update-target' },
        document: insertDocument('Doc', 'update-target', payloadDoc([['v', { '@type': 'xsd:boolean', '@value': true }]])).document,
      }
      const updateResult = await woql.post(agent, update).unverified()

      if (updateResult.status !== 200) {
        throw new Error(`WOQL UpdateDocument failed: ${JSON.stringify(updateResult.body)}`)
      }

      const docResult = await document.get(agent, {
        query: { id: 'Doc/update-target', as_list: true },
      })
      expect(docResult.body[0].payload).to.deep.equal({ v: true })
    })
  })

  describe('boolean class property via WOQL', function () {
    it('inserts a document with an xsd:boolean property', async function () {
      const query = {
        '@type': 'InsertDocument',
        document: dictTemplate([
          fieldValuePair('@type', value({ '@type': 'xsd:string', '@value': 'FlagDoc' })),
          fieldValuePair('name', value({ '@type': 'xsd:string', '@value': 'f1' })),
          fieldValuePair('flag', value({ '@type': 'xsd:boolean', '@value': true })),
        ]),
      }
      const insertResult = await woql.post(agent, query).unverified()

      if (insertResult.status !== 200) {
        throw new Error(`WOQL InsertDocument failed: ${JSON.stringify(insertResult.body)}`)
      }

      const docResult = await document.get(agent, {
        query: { id: 'FlagDoc/f1', as_list: true },
      })
      expect(docResult.body[0].flag).to.equal(true)
    })
  })
})
