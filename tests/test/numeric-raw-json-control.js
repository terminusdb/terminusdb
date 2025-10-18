const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')
const Decimal = require('decimal.js')

/**
 * Raw JSON String Control Tests
 *
 * CRITICAL: Test that uses raw JSON strings (bodyString parameter) to:
 * 1. Send exact JSON bytes on wire (bypass JavaScript's JSON.stringify)
 * 2. Extract response from raw text BEFORE JSON.parse() to preserve precision
 *
 * Pattern from decimal-precision.js lines 79-87:
 * - Extract numeric values from response.text using regex
 * - Convert to Decimal for precision comparison
 * - Verify raw JSON contains correct values
 *
 * JavaScript Precision Limitation:
 * - JavaScript numbers use IEEE 754 double (15-17 significant digits)
 * - Numbers with 20 digits lose precision: 12345678901234567890 → 12345678901234567000
 * - Solution: Extract from raw text, not from parsed body
 */

describe('numeric-raw-json-control', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Raw JSON - Request Control', function () {
    it('Send raw JSON with exact "2.0" string via bodyString', async function () {
      // Manually construct JSON string to control exact bytes on wire
      const rawJson = JSON.stringify({
        commit_info: { author: 'test', message: 'test' },
        query: {
          '@type': 'Typecast',
          value: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': '2.0' },
          },
          type: {
            '@type': 'NodeValue',
            node: 'xsd:decimal',
          },
          result: {
            '@type': 'Value',
            variable: 'Result',
          },
        },
        optimize: true,
      })

      // Send raw JSON string (bypasses JavaScript's JSON.stringify)
      const response = await woql.post(agent, null, { bodyString: rawJson }).unverified()

      // CRITICAL: Extract from raw text BEFORE JSON.parse()
      // Pattern from decimal-precision.js line 80
      const valueMatch = response.text.match(/"Result"[^}]*"@value"\s*:\s*([0-9.eE+-]+)/)
      expect(valueMatch, 'Could not extract @value from response').to.not.be.null

      const valueStr = valueMatch[1]
      const value = new Decimal(valueStr)

      // Verify canonical form: "2.0" → 2
      expect(value.equals(new Decimal('2'))).to.be.true
      expect(valueStr).to.equal('2') // Canonical: no .0
    })

    it('Send raw JSON with ".1" string via bodyString', async function () {
      const rawJson = JSON.stringify({
        commit_info: { author: 'test', message: 'test' },
        query: {
          '@type': 'Typecast',
          value: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': '.1' },
          },
          type: {
            '@type': 'NodeValue',
            node: 'xsd:decimal',
          },
          result: {
            '@type': 'Value',
            variable: 'Result',
          },
        },
        optimize: true,
      })

      const response = await woql.post(agent, null, { bodyString: rawJson }).unverified()

      // Extract from raw text to verify canonicalization
      const valueMatch = response.text.match(/"Result"[^}]*"@value"\s*:\s*([0-9.eE+-]+)/)
      expect(valueMatch).to.not.be.null

      const valueStr = valueMatch[1]
      const value = new Decimal(valueStr)

      // Should canonicalize ".1" to "0.1"
      expect(value.equals(new Decimal('0.1'))).to.be.true
    })

    it('Send raw JSON with arithmetic: ".1" + ".2"', async function () {
      const rawJson = JSON.stringify({
        commit_info: { author: 'test', message: 'test' },
        query: {
          '@type': 'Eval',
          expression: {
            '@type': 'Plus',
            left: {
              '@type': 'ArithmeticValue',
              data: { '@type': 'xsd:decimal', '@value': '.1' },
            },
            right: {
              '@type': 'ArithmeticValue',
              data: { '@type': 'xsd:decimal', '@value': '.2' },
            },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        },
        optimize: true,
      })

      const response = await woql.post(agent, null, { bodyString: rawJson }).unverified()

      // Extract from raw text
      const valueMatch = response.text.match(/"Result"[^}]*"@value"\s*:\s*([0-9.eE+-]+)/)
      expect(valueMatch).to.not.be.null

      const valueStr = valueMatch[1]
      const value = new Decimal(valueStr)

      // Should be exactly 0.3 (not 0.30000000000000004)
      // Rational arithmetic: 1/10 + 2/10 = 3/10 = 0.3 (exact)
      expect(value.equals(new Decimal('0.3'))).to.be.true
    })

    it('Raw JSON ensures "2.00" is sent as string, not number', async function () {
      // If we used JavaScript objects, 2.00 would become 2.00 (number) → "2" in JSON
      // With raw JSON string, we control exact representation
      const rawJson = `{
        "commit_info": {"author": "test", "message": "test"},
        "query": {
          "@type": "Typecast",
          "value": {
            "@type": "Value",
            "data": {"@type": "xsd:string", "@value": "2.00"}
          },
          "type": {
            "@type": "NodeValue",
            "node": "xsd:decimal"
          },
          "result": {
            "@type": "Value",
            "variable": "Result"
          }
        },
        "optimize": true
      }`

      const response = await woql.post(agent, null, { bodyString: rawJson }).unverified()

      // Extract from raw text
      const valueMatch = response.text.match(/"Result"[^}]*"@value"\s*:\s*([0-9.eE+-]+)/)
      expect(valueMatch).to.not.be.null

      const valueStr = valueMatch[1]
      const value = new Decimal(valueStr)

      // Server should canonicalize "2.00" to 2
      expect(value.equals(new Decimal('2'))).to.be.true
      expect(valueStr).to.equal('2') // Canonical: no trailing zeros
    })
  })

  describe('Raw JSON - Response Precision Limitation', function () {
    it('Demonstrates JavaScript precision loss on response (15-17 digits)', async function () {
      // We can send 20 digits as string, but response is parsed by JavaScript
      const rawJson = JSON.stringify({
        commit_info: { author: 'test', message: 'test' },
        query: {
          '@type': 'Typecast',
          value: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': '12345678901234567890' },
          },
          type: {
            '@type': 'NodeValue',
            node: 'xsd:decimal',
          },
          result: {
            '@type': 'Value',
            variable: 'Result',
          },
        },
        optimize: true,
      })

      const response = await woql.post(agent, null, { bodyString: rawJson }).unverified()

      // CRITICAL VERIFICATION: Check raw JSON response from server
      // Server should output full 20-digit JSON number
      expect(response.text).to.include('"@value":12345678901234567890')

      // Extract from raw text BEFORE JSON.parse()
      const valueMatch = response.text.match(/"Result"[^}]*"@value"\s*:\s*([0-9.eE+-]+)/)
      expect(valueMatch).to.not.be.null

      const valueStrFromText = valueMatch[1]
      console.log('Server sent (raw JSON):  ', valueStrFromText)

      // Convert to Decimal from string (preserves precision)
      const valueFromText = new Decimal(valueStrFromText)
      
      // Verify server sent full 20 digits
      expect(valueFromText.equals(new Decimal('12345678901234567890'))).to.be.true

      // DEMONSTRATE: Using JSON.parse() loses precision
      const parsedBody = JSON.parse(response.text)
      const valueFromParse = parsedBody.bindings[0].Result['@value']
      console.log('After JSON.parse():      ', valueFromParse.toString())

      // This demonstrates the precision loss
      expect(valueFromParse.toString()).to.equal('12345678901234567000') // Lost last 3 digits
    })

    it('Numbers within JavaScript precision range (15 digits) work fine', async function () {
      // 15 digits - within JavaScript's safe range
      const rawJson = JSON.stringify({
        commit_info: { author: 'test', message: 'test' },
        query: {
          '@type': 'Typecast',
          value: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': '123456789012345' },
          },
          type: {
            '@type': 'NodeValue',
            node: 'xsd:decimal',
          },
          result: {
            '@type': 'Value',
            variable: 'Result',
          },
        },
        optimize: true,
      })

      const response = await woql.post(agent, null, { bodyString: rawJson })

      const result = response.body.bindings[0].Result['@value']

      // 15 digits - no precision loss
      expect(result.toString()).to.equal('123456789012345')
    })
  })

  describe('Raw JSON - Complex Queries', function () {
    it('Raw JSON for comparison with exact canonical forms', async function () {
      const rawJson = `{
        "commit_info": {"author": "test", "message": "test"},
        "query": {
          "@type": "Equals",
          "left": {
            "@type": "DataValue",
            "data": {"@type": "xsd:decimal", "@value": "2.0"}
          },
          "right": {
            "@type": "DataValue",
            "data": {"@type": "xsd:decimal", "@value": "2.00"}
          }
        },
        "optimize": true
      }`

      const response = await woql.post(agent, null, { bodyString: rawJson })

      // Both should canonicalize to 2 and be equal
      expect(response.body.bindings).to.have.lengthOf(1)
    })

    it('Raw JSON for cross-type equality: integer "2" = decimal "2.0"', async function () {
      const rawJson = `{
        "commit_info": {"author": "test", "message": "test"},
        "query": {
          "@type": "Equals",
          "left": {
            "@type": "DataValue",
            "data": {"@type": "xsd:integer", "@value": "2"}
          },
          "right": {
            "@type": "DataValue",
            "data": {"@type": "xsd:decimal", "@value": "2.0"}
          }
        },
        "optimize": true
      }`

      const response = await woql.post(agent, null, { bodyString: rawJson })

      // Integer 2 should equal decimal 2.0
      expect(response.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('Raw JSON - Whitespace Variations', function () {
    it('Raw JSON can include explicit whitespace variations', async function () {
      // Demonstrate that we have byte-level control
      const rawJsonWithExtraSpaces = `{
        "commit_info": {  "author"  :  "test"  ,  "message"  :  "test"  }  ,
        "query": {
          "@type": "Typecast",
          "value": {
            "@type": "Value",
            "data": {"@type": "xsd:string", "@value": "2.0"}
          },
          "type": {
            "@type": "NodeValue",
            "node": "xsd:decimal"
          },
          "result": {
            "@type": "Value",
            "variable": "Result"
          }
        },
        "optimize": true
      }`

      // Extra whitespace should be ignored by server's JSON parser
      const response = await woql.post(agent, null, {
        bodyString: rawJsonWithExtraSpaces,
      })

      expect(response.body.bindings[0].Result['@value']).to.equal(2)
    })
  })
})
