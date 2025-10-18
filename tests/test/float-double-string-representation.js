const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

/**
 * Float/Double String Representation Test
 *
 * Issue: literal(33, "xsd:double") when typecast to string returns "33" 
 * instead of "33.0"
 *
 * Expected behavior:
 * - xsd:double and xsd:float should preserve decimal notation when 
 *   converted to string ("33.0" not "33")
 * - xsd:decimal can be "33" or "33.0" (both valid)
 * - xsd:integer should be "33" (no decimal)
 */

describe('float-double-string-representation', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Typecast to String - Float/Double Decimal Preservation', function () {
    it('literal(33, xsd:double) → string should be "33.0" not "33"', async function () {
      // Test xsd:double representation when typecast to string
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'Val' },
            right: { '@type': 'Value', data: { '@type': 'xsd:double', '@value': '33' } },
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'NodeValue', node: 'xsd:string' },
            result: { '@type': 'Value', variable: 'AsString' },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      expect(r.body.bindings).to.have.lengthOf(1)
      
      const stringValue = r.body.bindings[0].AsString['@value']
      
      // Verify: xsd:double should preserve decimal notation in string representation
      // Float/double values should preserve decimal notation
      expect(stringValue).to.match(/33\.0+$/, 
        'xsd:double should typecast to string with decimal point (e.g., "33.0")')
    })

    it('literal(33, xsd:float) → string should be "33.0" not "33"', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'Val' },
            right: { '@type': 'Value', data: { '@type': 'xsd:float', '@value': '33' } },
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'NodeValue', node: 'xsd:string' },
            result: { '@type': 'Value', variable: 'AsString' },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      expect(r.body.bindings).to.have.lengthOf(1)
      
      const stringValue = r.body.bindings[0].AsString['@value']
      
      // Verify: xsd:float should preserve decimal notation in string representation
      expect(stringValue).to.match(/33\.0+$/, 
        'xsd:float should typecast to string with decimal point (e.g., "33.0")')
    })

    it('literal(33, xsd:decimal) → string can be "33" (no decimal required)', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'Val' },
            right: { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': '33' } },
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'NodeValue', node: 'xsd:string' },
            result: { '@type': 'Value', variable: 'AsString' },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      expect(r.body.bindings).to.have.lengthOf(1)
      
      const stringValue = r.body.bindings[0].AsString['@value']
      
      // xsd:decimal can be "33" or "33.0" - both are valid
      // The implementation chooses to omit trailing zeros
      expect(stringValue).to.equal('33')
    })

    it('literal(33, xsd:integer) → string should be "33"', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'Val' },
            right: { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': '33' } },
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'NodeValue', node: 'xsd:string' },
            result: { '@type': 'Value', variable: 'AsString' },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      expect(r.body.bindings).to.have.lengthOf(1)
      
      const stringValue = r.body.bindings[0].AsString['@value']
      
      // Integer should never have decimal point
      expect(stringValue).to.equal('33')
    })
  })

  describe('Comprehensive Test - Multiple Types with Typecast', function () {
    it('CRITICAL: Numeric JSON value (not string) - xsd:double/float lose decimal notation', async function () {
      // THIS IS THE ACTUAL BUG!
      // When @value is a number (33) instead of string ("33"),
      // float/double lose their decimal notation in string representation
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Or',
            or: [
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:double', '@value': 33 } }, // NUMERIC value
              },
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:float', '@value': 33 } }, // NUMERIC value
              },
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': 33 } }, // NUMERIC value
              },
            ],
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'val' },
            type: { '@type': 'NodeValue', node: 'xsd:string' },
            result: { '@type': 'Value', variable: 'as_string' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'Value', variable: 'val' },
            type: { '@type': 'NodeValue', variable: 'type_of' },
          },
        ],
      }

      const r = await woql.post(agent, query)

      expect(r.body.bindings).to.have.lengthOf(3)

      // Helper to match type values (handles both full URI and shorthand)
      const matchesType = (typeOf, expectedType) => {
        const typeValue = typeof typeOf === 'string' ? typeOf : (typeOf && typeOf['@value'])
        return typeValue === expectedType ||
               typeValue === `xsd:${expectedType.split('#')[1]}`
      }

      const doubleBinding = r.body.bindings.find(b =>
        matchesType(b.type_of, 'http://www.w3.org/2001/XMLSchema#double'),
      )
      const floatBinding = r.body.bindings.find(b =>
        matchesType(b.type_of, 'http://www.w3.org/2001/XMLSchema#float'),
      )
      const decimalBinding = r.body.bindings.find(b =>
        matchesType(b.type_of, 'http://www.w3.org/2001/XMLSchema#decimal'),
      )

      expect(doubleBinding).to.exist
      expect(floatBinding).to.exist
      expect(decimalBinding).to.exist

      // Verify: xsd:double with numeric JSON value preserves decimal notation
      expect(doubleBinding.as_string['@value']).to.match(/33\.0+$/,
        'xsd:double with numeric JSON value should preserve decimal notation')

      // Verify: xsd:float with numeric JSON value preserves decimal notation
      expect(floatBinding.as_string['@value']).to.match(/33\.0+$/,
        'xsd:float with numeric JSON value should preserve decimal notation')

      // Verify: xsd:decimal correctly uses minimal form (no decimal required)
      expect(decimalBinding.as_string['@value']).to.equal('33')
    })

    it('Compare string representations of 33 across all numeric types', async function () {
      // This is the exact pattern from the user's issue
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Or',
            or: [
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'Val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:double', '@value': '33' } },
              },
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'Val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:float', '@value': '33' } },
              },
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'Val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': '33' } },
              },
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'Val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': '33' } },
              },
            ],
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'NodeValue', node: 'xsd:string' },
            result: { '@type': 'Value', variable: 'AsString' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'Value', variable: 'TypeOf' },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      // Should return 4 bindings (one for each type)
      expect(r.body.bindings).to.have.lengthOf(4)
      
      // Find each type (TypeOf might be an object with @value)
      const getTypeValue = (b) => {
        if (typeof b.TypeOf === 'string') return b.TypeOf
        if (b.TypeOf && b.TypeOf['@value']) return b.TypeOf['@value']
        return b.TypeOf
      }
      
      const matchesType = (typeOf, expectedType) => {
        const typeValue = getTypeValue({ TypeOf: typeOf })
        // Handle both "xsd:double" and full URI formats
        return typeValue === expectedType || 
               typeValue === `xsd:${expectedType.split('#')[1]}`
      }
      
      const doubleBinding = r.body.bindings.find(b => 
        matchesType(b.TypeOf, 'http://www.w3.org/2001/XMLSchema#double')
      )
      const floatBinding = r.body.bindings.find(b => 
        matchesType(b.TypeOf, 'http://www.w3.org/2001/XMLSchema#float')
      )
      const decimalBinding = r.body.bindings.find(b => 
        matchesType(b.TypeOf, 'http://www.w3.org/2001/XMLSchema#decimal')
      )
      const integerBinding = r.body.bindings.find(b => 
        matchesType(b.TypeOf, 'http://www.w3.org/2001/XMLSchema#integer')
      )
      
      expect(doubleBinding).to.exist
      expect(floatBinding).to.exist
      expect(decimalBinding).to.exist
      expect(integerBinding).to.exist
      
      // VERIFIED: Float and double correctly return "33.0"
      expect(doubleBinding.AsString['@value']).to.match(/33\.0+$/, 
        'xsd:double should preserve decimal notation in string representation')
      
      expect(floatBinding.AsString['@value']).to.match(/33\.0+$/, 
        'xsd:float should preserve decimal notation in string representation')
      
      // Decimal and integer correctly return "33" (no decimal)
      expect(decimalBinding.AsString['@value']).to.equal('33')
      expect(integerBinding.AsString['@value']).to.equal('33')
    })

    it('Compare string representations with fractional part (33.5)', async function () {
      // Test with a value that has a fractional part
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Or',
            or: [
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'Val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:double', '@value': '33.5' } },
              },
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'Val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:float', '@value': '33.5' } },
              },
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'Val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:decimal', '@value': '33.5' } },
              },
            ],
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'NodeValue', node: 'xsd:string' },
            result: { '@type': 'Value', variable: 'AsString' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'Value', variable: 'TypeOf' },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      expect(r.body.bindings).to.have.lengthOf(3)
      
      // All should preserve "33.5" (with fractional part, this works correctly)
      for (const binding of r.body.bindings) {
        expect(binding.AsString['@value']).to.match(/33\.5/, 
          'All types should preserve fractional part in string representation')
      }
    })
  })

  describe('Edge Cases - String Representation', function () {
    it('Zero values: 0 vs 0.0', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Or',
            or: [
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'Val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:double', '@value': '0' } },
              },
              {
                '@type': 'Equals',
                left: { '@type': 'Value', variable: 'Val' },
                right: { '@type': 'Value', data: { '@type': 'xsd:integer', '@value': '0' } },
              },
            ],
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'NodeValue', node: 'xsd:string' },
            result: { '@type': 'Value', variable: 'AsString' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'Value', variable: 'Val' },
            type: { '@type': 'Value', variable: 'TypeOf' },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      expect(r.body.bindings).to.have.lengthOf(2)
      
      const matchesType = (typeOf, expectedType) => {
        const typeValue = typeof typeOf === 'string' ? typeOf : (typeOf && typeOf['@value'])
        return typeValue === expectedType || 
               typeValue === `xsd:${expectedType.split('#')[1]}`
      }
      
      const doubleZero = r.body.bindings.find(b => 
        matchesType(b.TypeOf, 'http://www.w3.org/2001/XMLSchema#double')
      )
      const integerZero = r.body.bindings.find(b => 
        matchesType(b.TypeOf, 'http://www.w3.org/2001/XMLSchema#integer')
      )
      
      // Double zero should be "0.0"
      expect(doubleZero.AsString['@value']).to.match(/0\.0+$/, 
        'xsd:double zero should be "0.0"')
      
      // Integer zero should be "0"
      expect(integerZero.AsString['@value']).to.equal('0')
    })
  })
})
