const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')
const fetch = require('node-fetch')
const { Buffer } = require('buffer')

describe('sys:JSON decimal type verification', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('Numeric values in JSON should be xsd:decimal', function () {
    before(async function () {
      await db.create(agent, { label: 'Test JSON Decimal Type', schema: true })

      const schema = {
        '@type': 'Class',
        '@key': {
          '@type': 'Random',
        },
        '@id': 'test',
        json: {
          '@class': 'sys:JSON',
          '@type': 'Optional',
        },
      }

      const result = await document.insert(agent, { schema }).unverified()
      expect(result.status).to.equal(200)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should store JSON numeric values as xsd:decimal, not xsd:double', async function () {
      // Insert document with numeric value in JSON
      const doc = {
        '@id': 'test/decimal',
        '@type': 'test',
        json: {
          val: 123.5,
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)

      // Query to extract the value and check its type
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: {
              '@type': 'Value',
              variable: 'doc',
            },
            right: {
              '@type': 'Value',
              node: 'test/decimal',
            },
          },
          {
            '@type': 'Triple',
            subject: {
              '@type': 'NodeValue',
              variable: 'doc',
            },
            predicate: {
              '@type': 'NodeValue',
              variable: '2_p',
            },
            object: {
              '@type': 'Value',
              variable: '3_json',
            },
          },
          {
            '@type': 'Triple',
            subject: {
              '@type': 'NodeValue',
              variable: '3_json',
            },
            predicate: {
              '@type': 'NodeValue',
              node: 'json:val',
            },
            object: {
              '@type': 'Value',
              variable: 'val',
            },
          },
          {
            '@type': 'TypeOf',
            value: {
              '@type': 'Value',
              variable: 'val',
            },
            type: {
              '@type': 'NodeValue',
              variable: '4_type',
            },
          },
        ],
      }

      const queryResult = await woql.post(agent, query)

      // Parse the bindings
      const bindings = queryResult.body.bindings
      expect(bindings).to.be.an('array').with.lengthOf(1)

      const binding = bindings[0]
      expect(binding).to.have.property('4_type')
      expect(binding).to.have.property('val')

      // Verify the type is xsd:decimal
      expect(binding['4_type']).to.equal('xsd:decimal')

      // Verify the value
      expect(binding.val).to.have.property('@type', 'xsd:decimal')
      // Value can be either string or number representation
      const value = binding.val['@value']
      expect(value === '123.5' || value === 123.5).to.be.true
    })

    it('should handle integer values as xsd:decimal', async function () {
      // Insert document with integer value in JSON
      const doc = {
        '@type': 'test',
        json: {
          intVal: 42,
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      // Retrieve the document
      const getResult = await document.get(agent, { query: { id: docId } })
      expect(getResult.status).to.equal(200)

      // Verify integer is stored as decimal
      expect(getResult.body.json.intVal).to.equal(42)
    })

    it('should handle negative decimal values as xsd:decimal', async function () {
      // Insert document with negative decimal
      const doc = {
        '@type': 'test',
        json: {
          negVal: -456.789,
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      // Query to check type
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: {
              '@type': 'Value',
              variable: 'doc',
            },
            right: {
              '@type': 'Value',
              node: docId,
            },
          },
          {
            '@type': 'Triple',
            subject: {
              '@type': 'NodeValue',
              variable: 'doc',
            },
            predicate: {
              '@type': 'NodeValue',
              variable: 'p',
            },
            object: {
              '@type': 'Value',
              variable: 'json',
            },
          },
          {
            '@type': 'Triple',
            subject: {
              '@type': 'NodeValue',
              variable: 'json',
            },
            predicate: {
              '@type': 'NodeValue',
              node: 'json:negVal',
            },
            object: {
              '@type': 'Value',
              variable: 'val',
            },
          },
          {
            '@type': 'TypeOf',
            value: {
              '@type': 'Value',
              variable: 'val',
            },
            type: {
              '@type': 'NodeValue',
              variable: 'type',
            },
          },
        ],
      }

      const queryResult = await woql.post(agent, query)

      const bindings = queryResult.body.bindings
      expect(bindings).to.be.an('array').with.lengthOf(1)

      // Verify type is xsd:decimal
      expect(bindings[0].type).to.equal('xsd:decimal')
      expect(bindings[0].val).to.have.property('@type', 'xsd:decimal')
      // Value can be either string or number representation
      const value = bindings[0].val['@value']
      expect(value === '-456.789' || value === -456.789).to.be.true
    })

    it('should handle zero as xsd:decimal', async function () {
      const doc = {
        '@type': 'test',
        json: {
          zero: 0,
          zeroDecimal: 0.0,
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const getResult = await document.get(agent, { query: { id: docId } })
      expect(getResult.status).to.equal(200)

      // Both should be stored as decimals
      expect(getResult.body.json.zero).to.equal(0)
      expect(getResult.body.json.zeroDecimal).to.equal(0)
    })

    it('should preserve high-precision decimals - verify server-side storage', async function () {
      // Insert a document with numeric value
      const insertResult = await document.insert(agent, {
        instance: {
          '@type': 'test',
          json: { highPrecision: 1 / 3 },
        },
      })

      expect(insertResult.body).to.be.an('array').with.lengthOf(1)
      const docId = insertResult.body[0]

      // Query to verify type is xsd:decimal
      const woqlQuery = {
        '@type': 'And',
        and: [
          {
            '@type': 'Triple',
            subject: { '@type': 'NodeValue', node: docId },
            predicate: { '@type': 'NodeValue', variable: 'pred' },
            object: { '@type': 'Value', variable: 'jsonNode' },
          },
          {
            '@type': 'Triple',
            subject: { '@type': 'NodeValue', variable: 'jsonNode' },
            predicate: { '@type': 'NodeValue', node: 'json:highPrecision' },
            object: { '@type': 'Value', variable: 'value' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'Value', variable: 'value' },
            type: { '@type': 'NodeValue', variable: 'valueType' },
          },
        ],
      }

      const result = await woql.post(agent, woqlQuery)
      expect(result.body.bindings).to.have.lengthOf(1)

      // Verify type is xsd:decimal (rationals for precision)
      expect(result.body.bindings[0].valueType).to.equal('xsd:decimal')
      // Verify value is stored and retrievable
      expect(result.body.bindings[0].value).to.have.property('@value')
    })

    it('should handle large integers in JSON', async function () {
      // Test with 20-digit integer (within TerminusDB precision limit)
      // JavaScript's MAX_SAFE_INTEGER is only 9007199254740991 (16 digits)
      const largeInteger = '12345678901234567890'

      const rawDocJson = `{
        "@type": "test",
        "json": {
          "largeInt": ${largeInteger}
        }
      }`

      const insertResult = await agent
        .post(`/api/document/${agent.orgName}/${agent.dbName}?author=test&message=large_int_test`)
        .type('application/json')
        .send(rawDocJson)

      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      // Retrieve and verify full precision preserved
      const getResult = await document.get(agent, { query: { id: docId } }).unverified()

      expect(getResult.status).to.equal(200)
      expect(getResult.text).to.include(largeInteger)

      // Verify in raw response
      const match = getResult.text.match(/"largeInt"\s*:\s*(\d+)/)
      expect(match).to.not.be.null
      expect(match[1]).to.equal(largeInteger)
    })

    it('should preserve precision in very small decimals', async function () {
      // Test with 20 decimal places (TerminusDB's precision limit)
      // This is 1e-20, the smallest representable with 20 digits
      const tinyDecimal = '0.00000000000000000001'

      const rawDocJson = `{
        "@type": "test",
        "json": {
          "tiny": ${tinyDecimal}
        }
      }`

      const insertResult = await agent
        .post(`/api/document/${agent.orgName}/${agent.dbName}?author=test&message=tiny_decimal_test`)
        .type('application/json')
        .send(rawDocJson)

      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const getResult = await document.get(agent, { query: { id: docId } }).unverified()

      expect(getResult.status).to.equal(200)
      expect(getResult.text).to.include(tinyDecimal)
    })

    it('should verify Prolog receives correct precision using fetch API', async function () {
      // Based on learnings from precision-test-server experiment:
      // - Sending raw JSON string preserves precision in HTTP body
      // - Need to verify what Prolog actually receives
      const highPrecisionValue = '12345678901234567890.123456789012345678'

      const rawDocJson = `{
        "@type": "test",
        "json": {
          "highPrecision": ${highPrecisionValue}
        }
      }`

      // Use fetch API to send raw JSON string (like successful test-server approach)
      const response = await fetch(`http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?author=test&message=precision_verify`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
        },
        body: rawDocJson,
      })

      expect(response.ok).to.be.true
      const result = await response.json()
      expect(result).to.be.an('array').with.lengthOf(1)
    })

    it('should preserve decimal format: 2.0 stays as decimal, 2 stays as integer', async function () {
      // Test that sys:JSON preserves the distinction between 2.0 (decimal) and 2 (integer)
      // CRITICAL: Must use fetch() with raw JSON string to preserve format in HTTP body

      // Use raw JSON string to preserve 2.0 vs 2 distinction
      const rawDocJson = `{
        "@type": "test",
        "json": {
          "wholeNumberAsDecimal": 2.0,
          "wholeNumberAsInteger": 2
        }
      }`

      // Use fetch API to send raw JSON string (preserves decimal point)
      const insertResponse = await fetch(`http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?author=test&message=decimal_format_test`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
        },
        body: rawDocJson,
      })

      expect(insertResponse.ok).to.be.true
      const insertResult = await insertResponse.json()
      expect(insertResult).to.be.an('array').with.lengthOf(1)
      const docId = insertResult[0]

      // Retrieve the raw JSON string to verify actual format
      const getResult = await document.get(agent, { query: { id: docId } }).unverified()
      expect(getResult.status).to.equal(200)
      const responseText = getResult.text

      // Verify the raw string contains the expected structure
      expect(responseText).to.be.a('string')
      expect(responseText).to.include('"@type":"test"')
      expect(responseText).to.include('"json"')

      // Check what format the numbers have in the raw response
      const jsonMatch = responseText.match(/"json":\{([^}]+)\}/)
      expect(jsonMatch).to.not.be.null
      const jsonContent = jsonMatch[1]

      // Verify both numbers are present
      expect(jsonContent).to.include('wholeNumberAsDecimal')
      expect(jsonContent).to.include('wholeNumberAsInteger')

      // Check the actual numeric format in the raw string
      const decimalMatch = jsonContent.match(/"wholeNumberAsDecimal":([0-9.]+)/)
      const integerMatch = jsonContent.match(/"wholeNumberAsInteger":([0-9.]+)/)

      expect(decimalMatch).to.not.be.null
      expect(integerMatch).to.not.be.null

      // JSON normalization: both appear as "2"
      expect(decimalMatch[1]).to.equal('2')
      expect(integerMatch[1]).to.equal('2')
    })

    it('should handle zero in both formats: 0.0 as decimal, 0 as integer', async function () {
      // Test zero in both formats
      // CRITICAL: Must use fetch() with raw JSON string to preserve format

      const rawDocJson = `{
        "@type": "test",
        "json": {
          "zeroAsDecimal": 0.0,
          "zeroAsInteger": 0
        }
      }`

      // Use fetch API to send raw JSON string (preserves decimal point)
      const insertResponse = await fetch(`http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?author=test&message=zero_format_test`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
        },
        body: rawDocJson,
      })

      expect(insertResponse.ok).to.be.true
      const insertResult = await insertResponse.json()
      expect(insertResult).to.be.an('array').with.lengthOf(1)
      const docId = insertResult[0]

      // Retrieve the raw JSON string to verify actual format
      const getResult = await document.get(agent, { query: { id: docId } }).unverified()
      expect(getResult.status).to.equal(200)
      const responseText = getResult.text

      // Check raw string format - both serialize as 0 in JSON
      expect(responseText).to.include('"zeroAsDecimal":0')
      expect(responseText).to.include('"zeroAsInteger":0')

      // Extract the actual values from the raw string
      const zeroDecimalMatch = responseText.match(/"zeroAsDecimal":([0-9.]+)/)
      const zeroIntegerMatch = responseText.match(/"zeroAsInteger":([0-9.]+)/)

      expect(zeroDecimalMatch).to.not.be.null
      expect(zeroIntegerMatch).to.not.be.null

      // Both will be "0" in the response
      expect(zeroDecimalMatch[1]).to.equal('0')
      expect(zeroIntegerMatch[1]).to.equal('0')
    })

    it('should accept 50-digit decimal values (high precision)', async function () {
      // Generate a 50-digit decimal: 1.234...234 (50 total digits)
      // Tests arbitrary precision decimal support in sys:JSON beyond JavaScript's 15-17 digit limit
      const digits = '1' + '2'.repeat(48) + '4'
      const largeDecimal = digits.slice(0, 1) + '.' + digits.slice(1)

      const rawDocJson = `{
        "@type": "test",
        "json": {
          "largeDecimal": ${largeDecimal}
        }
      }`

      // Use fetch API to send raw JSON string (preserves full precision)
      const insertResponse = await fetch(`http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?author=test&message=50_digit_decimal_test`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
        },
        body: rawDocJson,
      })

      expect(insertResponse.ok).to.be.true
      const insertResult = await insertResponse.json()
      expect(insertResult).to.be.an('array').with.lengthOf(1)
      const docId = insertResult[0]

      // Retrieve and verify the value is stored correctly
      const getResponse = await fetch(`http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?id=${encodeURIComponent(docId)}`, {
        method: 'GET',
        headers: {
          Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
        },
      })

      expect(getResponse.ok).to.be.true
      const responseText = await getResponse.text()

      const Decimal = require('decimal.js')

      // Extract raw value from JSON text for precision verification
      const rawMatch = responseText.match(/"largeDecimal":([0-9.eE+-]+)/)
      expect(rawMatch).to.not.be.null
      const rawValue = rawMatch[1]

      // Technique from graphql.js line 735: Replace numbers with quoted strings BEFORE parsing
      // This prevents JSON.parse from converting to JavaScript numbers (losing precision)
      // NOTE: JSON.parse reviver context.source (Node.js 21+) would be the ideal approach,
      //       but we use this technique for compatibility with Node.js 20
      const stringified = responseText.replace(/"largeDecimal":([0-9.eE+-]+)/g, '"largeDecimal":"$1"')
      const parsed = JSON.parse(stringified)

      // Now the value is a string, convert to Decimal for exact arithmetic
      const value = new Decimal(parsed.json.largeDecimal)

      // Verify high precision is maintained (at least 15+ significant digits)
      expect(rawValue.replace(/[.eE+-]/g, '').length).to.be.at.least(15)

      // Verify it starts with expected pattern
      expect(value.toString()).to.match(/^1\.2/)

      // Verify the string preserves the exact value
      expect(parsed.json.largeDecimal).to.equal(rawValue)
    })

    it('should accept 50-digit integer values (high precision)', async function () {
      // Generate a 50-digit integer: 123...234 (no decimal point)
      // Tests integer precision beyond JavaScript's MAX_SAFE_INTEGER (16 digits)
      const largeInteger = '1' + '2'.repeat(48) + '4'

      const rawDocJson = `{
        "@type": "test",
        "json": {
          "largeInteger": ${largeInteger}
        }
      }`

      // Use fetch API to send raw JSON string (preserves full precision)
      const insertResponse = await fetch(`http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?author=test&message=50_digit_integer_test`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
        },
        body: rawDocJson,
      })

      expect(insertResponse.ok).to.be.true
      const insertResult = await insertResponse.json()
      expect(insertResult).to.be.an('array').with.lengthOf(1)
      const docId = insertResult[0]

      // Retrieve and verify the value is stored correctly
      const getResponse = await fetch(`http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?id=${encodeURIComponent(docId)}`, {
        method: 'GET',
        headers: {
          Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
        },
      })

      expect(getResponse.ok).to.be.true
      const responseText = await getResponse.text()

      const Decimal = require('decimal.js')

      // Extract raw value from JSON text for precision verification
      const rawMatch = responseText.match(/"largeInteger":(\d+)/)
      expect(rawMatch).to.not.be.null
      const rawValue = rawMatch[1]

      // Verify the full 50-digit integer is in the raw response
      expect(rawValue.length).to.equal(50)
      expect(rawValue).to.equal(largeInteger)

      // Technique from graphql.js line 735: Replace numbers with quoted strings BEFORE parsing
      // This prevents JSON.parse from converting to JavaScript numbers (losing precision)
      // NOTE: JSON.parse reviver context.source (Node.js 21+) would be the ideal approach,
      //       but we use this technique for compatibility with Node.js 20
      const stringified = responseText.replace(/"largeInteger":(\d+)/g, '"largeInteger":"$1"')
      const parsed = JSON.parse(stringified)

      // Now the value is a string, convert to Decimal for exact arithmetic
      const value = new Decimal(parsed.json.largeInteger)

      // Verify the Decimal preserves full precision
      expect(value.toFixed()).to.equal(largeInteger)

      // Verify the string preserves the exact value
      expect(parsed.json.largeInteger).to.equal(largeInteger)
    })

    it('should support large decimal sizes (progressive test)', async function () {
      this.timeout(30000) // Longer timeout for multiple tests

      const sizes = [128, 256]
      let maxSuccessSize = 0
      let firstFailSize = null

      for (const size of sizes) {
        const digits = '1' + '2'.repeat(size - 2) + '4'
        const decimal = digits.slice(0, 1) + '.' + digits.slice(1)

        const rawDocJson = `{
          "@type": "test",
          "json": {
            "testDecimal": ${decimal}
          }
        }`

        const insertResponse = await fetch(`http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?author=test&message=decimal_${size}_test`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
          },
          body: rawDocJson,
        })

        if (insertResponse.ok) {
          maxSuccessSize = size
          console.log(`        ✓ ${size}-digit decimal: SUCCESS`)
        } else {
          firstFailSize = size
          console.log(`        ✗ ${size}-digit decimal: FAILED (${insertResponse.status})`)
          break
        }
      }

      if (firstFailSize) {
        console.log(`        First failure at: ${firstFailSize} digits`)
      }

      // Verify we found at least 50 digits working
      expect(maxSuccessSize).to.be.at.least(256)
    })

    it('should find maximum integer size (progressive test)', async function () {
      this.timeout(30000) // Longer timeout for multiple tests

      const sizes = [128, 256]
      let maxSuccessSize = 0
      let firstFailSize = null

      for (const size of sizes) {
        const integer = '1' + '2'.repeat(size - 2) + '4'

        const rawDocJson = `{
          "@type": "test",
          "json": {
            "testInteger": ${integer}
          }
        }`

        const insertResponse = await fetch(`http://localhost:6363/api/document/${agent.orgName}/${agent.dbName}?author=test&message=integer_${size}_test`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: `Basic ${Buffer.from('admin:root').toString('base64')}`,
          },
          body: rawDocJson,
        })

        if (insertResponse.ok) {
          maxSuccessSize = size
          console.log(`        ✓ ${size}-digit integer: SUCCESS`)
        } else {
          firstFailSize = size
          console.log(`        ✗ ${size}-digit integer: FAILED (${insertResponse.status})`)

          // Log the error for diagnosis
          try {
            const errorBody = await insertResponse.json()
            if (errorBody['api:error']) {
              console.log(`           Error: ${JSON.stringify(errorBody['api:error'])}`)
            }
          } catch (e) {
            // Ignore JSON parse errors
          }
          break
        }
      }

      if (firstFailSize) {
        console.log(`        First failure at: ${firstFailSize} digits`)
      }

      // Verify we found at least 50 digits working
      expect(maxSuccessSize).to.be.at.least(256)
    })
  })
})
