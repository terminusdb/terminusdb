const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')
const fetch = require('node-fetch')
const { Buffer } = require('buffer')
const Decimal = require('decimal.js')

describe('sys:JSON Typecast', function () {
  let agent

  const schema = {
    '@type': 'Class',
    '@key': { '@type': 'Random' },
    '@id': 'test',
    json: {
      '@class': 'sys:JSON',
      '@type': 'Optional',
    },
  }

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent, { label: 'Test JSON Typecast', schema: true })
    const schemaResult = await document.insert(agent, { schema }).unverified()
    expect(schemaResult.status).to.equal(200)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('typecast sys:JSON rational to xsd:decimal preserves 20 decimal places', async function () {
    // Value with 40 decimal places (not ending in zero) - TerminusDB preserves 20 decimal places
    const inputValue = '1.0123456789012345678901234567890123456789'
    // Expected: exactly 20 decimal places preserved
    const expectedValue = '1.01234567890123456789'

    // Insert document with high-precision numeric value using raw JSON
    const rawDocJson = `{
      "@id": "test/highprecision",
      "@type": "test",
      "json": {
        "amount": ${inputValue}
      }
    }`

    const insertResponse = await fetch(
      `http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?author=test&message=highprecision`,
      {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
        },
        body: rawDocJson,
      },
    )
    expect(insertResponse.ok).to.be.true

    // Query with typecast to xsd:decimal
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Equals',
          left: { '@type': 'Value', variable: 'doc' },
          right: { '@type': 'Value', node: 'test/highprecision' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'doc' },
          predicate: { '@type': 'NodeValue', variable: 'pred' },
          object: { '@type': 'Value', variable: 'jsonNode' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'jsonNode' },
          predicate: { '@type': 'NodeValue', node: 'json:amount' },
          object: { '@type': 'Value', variable: 'amount' },
        },
        {
          '@type': 'Typecast',
          value: { '@type': 'Value', variable: 'amount' },
          type: { '@type': 'NodeValue', node: 'xsd:decimal' },
          result: { '@type': 'Value', variable: 'amount_dec' },
        },
      ],
    }

    const response = await woql.post(agent, query).unverified()
    expect(response.status).to.equal(200)

    // Check the raw response text to avoid JavaScript float precision loss
    const rawText = response.text
    expect(rawText).to.include('api:success')

    // Extract the @value from raw text using regex
    // The response contains "amount_dec":{"@type":"xsd:decimal","@value":...}
    const valueMatch = rawText.match(/"amount_dec":\s*\{\s*"@type"\s*:\s*"xsd:decimal"\s*,\s*"@value"\s*:\s*"?([^",}]+)"?/)
    expect(valueMatch).to.not.be.null
    const returnedValueStr = valueMatch[1]

    // Verify exact match of 20 decimal places
    expect(returnedValueStr).to.equal(expectedValue)
  })

  it('typecast sys:JSON rational to xsd:string', async function () {
    // Insert document with numeric value
    const doc = {
      '@id': 'test/string_cast',
      '@type': 'test',
      json: {
        amount: 1523.47,
      },
    }
    const docResult = await document.insert(agent, { instance: doc }).unverified()
    expect(docResult.status).to.equal(200)

    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Equals',
          left: { '@type': 'Value', variable: 'doc' },
          right: { '@type': 'Value', node: 'test/string_cast' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'doc' },
          predicate: { '@type': 'NodeValue', variable: 'pred' },
          object: { '@type': 'Value', variable: 'jsonNode' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'jsonNode' },
          predicate: { '@type': 'NodeValue', node: 'json:amount' },
          object: { '@type': 'Value', variable: 'amount' },
        },
        {
          '@type': 'Typecast',
          value: { '@type': 'Value', variable: 'amount' },
          type: { '@type': 'NodeValue', node: 'xsd:string' },
          result: { '@type': 'Value', variable: 'amount_str' },
        },
      ],
    }

    const response = await woql.post(agent, query).unverified()
    expect(response.status).to.equal(200)
    expect(response.body['api:status']).to.equal('api:success')

    const bindings = response.body.bindings
    expect(bindings).to.be.an('array').with.length.greaterThan(0)

    const result = bindings[0]
    expect(result.amount_str).to.exist
    expect(result.amount_str['@type']).to.equal('xsd:string')

    // Use decimal.js for exact comparison
    const returnedStr = result.amount_str['@value']
    const returnedDecimal = new Decimal(returnedStr)
    const expectedDecimal = new Decimal('1523.47')
    expect(returnedDecimal.equals(expectedDecimal)).to.be.true
  })
})
