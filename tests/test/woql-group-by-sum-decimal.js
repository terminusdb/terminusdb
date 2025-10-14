const { expect } = require('chai')
const { Agent, db, woql, document } = require('../lib')

describe('woql-group-by-sum-decimal', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('setup schema and data', async function () {
    // Add schema
    const schemaDoc = {
      '@type': 'Class',
      '@key': {
        '@type': 'Random',
      },
      '@id': 'Number',
      unsignedShort: 'xsd:unsignedShort',
      shortInt: 'xsd:short',
      label: 'xsd:string',
      int: 'xsd:integer',
      decimal: 'xsd:decimal',
      decimalWithDecimals: 'xsd:decimal',
      negative: 'xsd:decimal',
    }

    await document.insert(agent, { schema: schemaDoc })

    // Add instance data - using EXACT data from bug report
    const instanceDocs = [
      {
        '@type': 'Number',
        '@id': 'Number/2',
        decimal: '2', // Works - whole number
        int: '2',
        shortInt: 2,
        unsignedShort: 2,
        label: 'same',
        decimalWithDecimals: '2.0', // FAILS - decimal point
        negative: '-2', // FAILS - negative
      },
      {
        '@type': 'Number',
        '@id': 'Number/4',
        decimal: '4', // Works - whole number
        int: '4',
        shortInt: 4,
        unsignedShort: 4,
        label: 'same',
        decimalWithDecimals: '4.0', // FAILS - decimal point (note bug report has "2.0" here)
        negative: '-4', // FAILS - negative
      },
    ]

    await document.insert(agent, { instance: instanceDocs })
  })

  it('passes group_by with sum on integers (whole numbers)', async function () {
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'GroupBy',
          group_by: ['label'],
          template: ['decimal'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: 'rdf:type' },
                object: { '@type': 'Value', node: '@schema:Number' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:label' },
                object: { '@type': 'Value', variable: 'label' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:decimal' },
                object: { '@type': 'Value', variable: 'decimal' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:int' },
                object: { '@type': 'Value', variable: 'int' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:shortInt' },
                object: { '@type': 'Value', variable: 'shortInt' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:unsignedShort' },
                object: { '@type': 'Value', variable: 'unsignedShort' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:decimalWithDecimals' },
                object: { '@type': 'Value', variable: 'decimalWithDecimals' },
              },
            ],
          },
        },
        {
          '@type': 'Sum',
          list: { '@type': 'DataValue', variable: 'result' },
          result: { '@type': 'Value', variable: 'sum' },
        },
      ],
    }

    const rawResponse = await woql.post(agent, query).unverified()
    if (rawResponse.status !== 200) {
      console.log('\n=== ERROR RESPONSE ===')
      console.log('Status:', rawResponse.status)
      console.log('Body:', JSON.stringify(rawResponse.body, null, 2))
      console.log('======================\n')
    }
    expect(rawResponse.status).to.equal(200)
    expect(rawResponse.body.bindings).to.be.an('array').that.has.lengthOf(1)
    expect(rawResponse.body.bindings[0].sum['@value']).to.equal(6)
  })

  it('passes group_by with sum on decimals with decimal points', async function () {
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'GroupBy',
          group_by: ['label'],
          template: ['decimalWithDecimals'], // Array of strings as in GitHub issue
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: 'rdf:type' },
                object: { '@type': 'Value', node: '@schema:Number' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:label' },
                object: { '@type': 'Value', variable: 'label' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:decimal' },
                object: { '@type': 'Value', variable: 'decimal' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:int' },
                object: { '@type': 'Value', variable: 'int' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:shortInt' },
                object: { '@type': 'Value', variable: 'shortInt' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:unsignedShort' },
                object: { '@type': 'Value', variable: 'unsignedShort' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:decimalWithDecimals' },
                object: { '@type': 'Value', variable: 'decimalWithDecimals' },
              },
            ],
          },
        },
        {
          '@type': 'Sum',
          list: { '@type': 'DataValue', variable: 'result' }, // DataValue as in GitHub issue
          result: { '@type': 'Value', variable: 'sum' },
        },
      ],
    }

    const response = await woql.post(agent, query).unverified()
    if (response.status !== 200) {
      console.log('\n=== ERROR RESPONSE (decimals) ===')
      console.log('Status:', response.status)
      console.log('Body:', JSON.stringify(response.body, null, 2))
      console.log('======================\n')
    }
    expect(response.status).to.equal(200)
    expect(response.body.bindings).to.be.an('array').that.has.lengthOf(1)
    // 2.0 + 4.0 = 6.0
    expect(response.body.bindings[0].sum['@value']).to.equal(6)
  })

  it('passes group_by with sum on negative numbers', async function () {
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'GroupBy',
          group_by: ['label'],
          template: ['negative'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: 'rdf:type' },
                object: { '@type': 'Value', node: '@schema:Number' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:label' },
                object: { '@type': 'Value', variable: 'label' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:decimal' },
                object: { '@type': 'Value', variable: 'decimal' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:int' },
                object: { '@type': 'Value', variable: 'int' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:shortInt' },
                object: { '@type': 'Value', variable: 'shortInt' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:negative' },
                object: { '@type': 'Value', variable: 'negative' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:unsignedShort' },
                object: { '@type': 'Value', variable: 'unsignedShort' },
              },
              {
                '@type': 'Triple',
                subject: { '@type': 'NodeValue', variable: 'subject' },
                predicate: { '@type': 'NodeValue', node: '@schema:decimalWithDecimals' },
                object: { '@type': 'Value', variable: 'decimalWithDecimals' },
              },
            ],
          },
        },
        {
          '@type': 'Sum',
          list: { '@type': 'DataValue', variable: 'result' },
          result: { '@type': 'Value', variable: 'sum' },
        },
      ],
    }

    const response = await woql.post(agent, query).unverified()
    if (response.status !== 200) {
      console.log('\n=== ERROR RESPONSE (negative) ===')
      console.log('Status:', response.status)
      console.log('Body:', JSON.stringify(response.body, null, 2))
      console.log('======================\n')
    }
    expect(response.status).to.equal(200)
    expect(response.body.bindings).to.be.an('array').that.has.lengthOf(1)
    // -2 + -4 = -6
    expect(response.body.bindings[0].sum['@value']).to.equal(-6)
  })
})
