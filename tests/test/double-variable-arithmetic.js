const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('double-variable-arithmetic', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Type Preservation vs Arithmetic Behavior', function () {
    it('variables retain xsd:double type (verified with TypeOf)', async function () {
      // This test proves that variables ARE correctly typed as xsd:double
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'x' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '0.1' },
            },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'x' },
            type: { '@type': 'DataValue', variable: 'x_type' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      expect(result.bindings).to.be.an('array').with.lengthOf(1)
      const binding = result.bindings[0]

      expect(binding.x_type).to.equal('xsd:double',
        'Variable should retain xsd:double type')
    })

    it('arithmetic on double variables uses binary floating point arithmetic', async function () {
      // This test demonstrates the BUG: arithmetic on double variables
      // uses rational arithmetic instead of float arithmetic
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'x' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '0.1' },
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'y' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '0.2' },
            },
          },
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Plus',
              left: { '@type': 'ArithmeticValue', variable: 'x' },
              right: { '@type': 'ArithmeticValue', variable: 'y' },
            },
            result: { '@type': 'ArithmeticValue', variable: 'result' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'result' },
            type: { '@type': 'DataValue', variable: 'result_type' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      expect(result.bindings).to.be.an('array').with.lengthOf(1)
      const binding = result.bindings[0]

      // ❌ This FAILS - result type is xsd:decimal (rational arithmetic)
      // Expected: xsd:double (float arithmetic)
      expect(binding.result_type).to.equal('xsd:double')

      // Despite using rational arithmetic, the VALUE still shows IEEE 754 precision loss
      // This proves the values were floats at some point
      expect(binding.result['@value']).to.be.closeTo(0.30000000000000004, 0.0000000000000001)
    })

    it('inline double values use float arithmetic', async function () {
      // This test shows the CORRECT behavior with inline values
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Plus',
              left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
              right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.2' } },
            },
            result: { '@type': 'ArithmeticValue', variable: 'result' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'result' },
            type: { '@type': 'DataValue', variable: 'result_type' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      expect(result.bindings).to.be.an('array').with.lengthOf(1)
      const binding = result.bindings[0]

      // ✅ This PASSES - inline values use float arithmetic
      expect(binding.result_type).to.equal('xsd:double',
        'Inline double values correctly use float arithmetic')

      expect(binding.result['@value']).to.be.closeTo(0.30000000000000004, 0.0000000000000001)
    })

    it('Multiplication of double variables uses float arithmetic', async function () {
      // This test helps understand WHY the bug occurs
      // When compile_arith calls literally(), it strips ^^Type annotations
      // So by the time arithmetic happens, we have bare Prolog values
      // xsd:double floats become bare floats, which ARE checked by our fix
      // But variables don't go through the same path

      const query = {
        '@type': 'And',
        and: [
          // Assign two doubles to variables
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'x' },
            right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '2.5' } },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'y' },
            right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '4.0' } },
          },
          // Multiply them (should use float arithmetic)
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Times',
              left: { '@type': 'ArithmeticValue', variable: 'x' },
              right: { '@type': 'ArithmeticValue', variable: 'y' },
            },
            result: { '@type': 'ArithmeticValue', variable: 'result' },
          },
          // Check the result type
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'result' },
            type: { '@type': 'DataValue', variable: 'result_type' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      const binding = result.bindings[0]

      // Old behavior: xsd:decimal (rational arithmetic)
      // Expected behavior: xsd:double (float arithmetic)
      expect(binding.result_type).to.equal('xsd:double')
      expect(binding.result['@value']).to.equal(10.0)
    })
  })

  describe('Performance Implications of Query Processing', function () {
    // These tests demonstrate performance characteristics of different arithmetic operations
    // Network overhead may dwarf arithmetic differences, but patterns are still visible
    const iterations = 100

    it('double arithmetic with inline values', async function () {
      // IEEE 754 float arithmetic with inline values
      for (let i = 0; i < iterations; i++) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
            right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.2' } },
          },
          result: { '@type': 'ArithmeticValue', variable: 'result' },
        }
        await woql.post(agent, query).unverified()
      }
    }).timeout(30000)

    it('double arithmetic with variables', async function () {
      // IEEE 754 float arithmetic with variable binding overhead
      for (let i = 0; i < iterations; i++) {
        const query = {
          '@type': 'And',
          and: [
            {
              '@type': 'Equals',
              left: { '@type': 'DataValue', variable: 'x' },
              right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
            },
            {
              '@type': 'Equals',
              left: { '@type': 'DataValue', variable: 'y' },
              right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '0.2' } },
            },
            {
              '@type': 'Eval',
              expression: {
                '@type': 'Plus',
                left: { '@type': 'ArithmeticValue', variable: 'x' },
                right: { '@type': 'ArithmeticValue', variable: 'y' },
              },
              result: { '@type': 'ArithmeticValue', variable: 'result' },
            },
          ],
        }
        await woql.post(agent, query).unverified()
      }
    }).timeout(30000)

    it('decimal arithmetic with inline values', async function () {
      // GMP rational arithmetic with inline values (slower than float)
      for (let i = 0; i < iterations; i++) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } },
            right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } },
          },
          result: { '@type': 'ArithmeticValue', variable: 'result' },
        }
        await woql.post(agent, query).unverified()
      }
    }).timeout(30000)

    it('decimal arithmetic with variables', async function () {
      // GMP rational arithmetic with variable binding overhead
      for (let i = 0; i < iterations; i++) {
        const query = {
          '@type': 'And',
          and: [
            {
              '@type': 'Equals',
              left: { '@type': 'DataValue', variable: 'x' },
              right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } },
            },
            {
              '@type': 'Equals',
              left: { '@type': 'DataValue', variable: 'y' },
              right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } },
            },
            {
              '@type': 'Eval',
              expression: {
                '@type': 'Plus',
                left: { '@type': 'ArithmeticValue', variable: 'x' },
                right: { '@type': 'ArithmeticValue', variable: 'y' },
              },
              result: { '@type': 'ArithmeticValue', variable: 'result' },
            },
          ],
        }
        await woql.post(agent, query).unverified()
      }
    }).timeout(30000)
  })

  describe('Bulk Arithmetic Performance (4M solutions per query)', function () {
    // These tests generate 1000x1000 = 1,000,000 solutions in a SINGLE query
    // Member variables create the cartesian product but arithmetic is on FIXED values (0.1 and 0.2)
    // This tests the overhead of generating solutions while performing the same calculation

    let range2000
    const count = 2000
    const totalCount = count * count

    before(function () {
      // Helper to generate array of integers for Member list (just for cardinality)
      const generateRange = (n) => {
        const arr = []
        for (let i = 1; i <= n; i++) {
          arr.push({
            '@type': 'Value',
            data: { '@type': 'xsd:integer', '@value': i },
          })
        }
        return arr
      }
      range2000 = generateRange(count)
    })

    // Double with inline values
    it('double addition (inline)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Plus',
                  left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': 0.1 } },
                  right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': 0.2 } },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'x' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'y' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('double subtraction (inline)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Minus',
                  left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': 0.2 } },
                  right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': 0.1 } },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'x' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'y' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('double multiplication (inline)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Times',
                  left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': 0.1 } },
                  right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': 0.2 } },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'x' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'y' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('double division (inline)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Divide',
                  left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': 0.2 } },
                  right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': 0.1 } },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'x' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'y' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    // Decimal with inline values
    it('decimal addition (inline)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Plus',
                  left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } },
                  right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'x' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'y' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('decimal subtraction (inline)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Minus',
                  left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } },
                  right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'x' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'y' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('decimal multiplication (inline)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Times',
                  left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } },
                  right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'x' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'y' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('decimal division (inline)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Divide',
                  left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } },
                  right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'x' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'y' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    // Double with variables
    it('double addition (variables)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'a' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': 0.1 } } },
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'b' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': 0.2 } } },
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Plus',
                  left: { '@type': 'ArithmeticValue', variable: 'a' },
                  right: { '@type': 'ArithmeticValue', variable: 'b' },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'i' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'j' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('double subtraction (variables)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'a' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': 0.2 } } },
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'b' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': 0.1 } } },
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Minus',
                  left: { '@type': 'ArithmeticValue', variable: 'a' },
                  right: { '@type': 'ArithmeticValue', variable: 'b' },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'i' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'j' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('double multiplication (variables)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'a' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': 0.1 } } },
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'b' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': 0.2 } } },
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Times',
                  left: { '@type': 'ArithmeticValue', variable: 'a' },
                  right: { '@type': 'ArithmeticValue', variable: 'b' },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'i' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'j' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('double division (variables)', async function () {
      // NOW WORKS! New logic: defaults to / when types unknown (safer)
      // Only uses rdiv when BOTH args are provably rational
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'a' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': 0.2 } } },
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'b' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': 0.1 } } },
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Divide',
                  left: { '@type': 'ArithmeticValue', variable: 'a' },
                  right: { '@type': 'ArithmeticValue', variable: 'b' },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'i' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'j' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    // Decimal with variables
    it('decimal addition (variables)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'a' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } } },
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'b' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } } },
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Plus',
                  left: { '@type': 'ArithmeticValue', variable: 'a' },
                  right: { '@type': 'ArithmeticValue', variable: 'b' },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'i' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'j' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('decimal subtraction (variables)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'a' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } } },
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'b' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } } },
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Minus',
                  left: { '@type': 'ArithmeticValue', variable: 'a' },
                  right: { '@type': 'ArithmeticValue', variable: 'b' },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'i' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'j' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('decimal multiplication (variables)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'a' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } } },
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'b' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } } },
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Times',
                  left: { '@type': 'ArithmeticValue', variable: 'a' },
                  right: { '@type': 'ArithmeticValue', variable: 'b' },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'i' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'j' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)

    it('decimal division (variables)', async function () {
      const query = {
        '@type': 'Select',
        variables: [''],
        query: {
          '@type': 'GroupBy',
          group_by: ['none'],
          template: ['result'],
          grouped: { '@type': 'Value', variable: 'result' },
          query: {
            '@type': 'And',
            and: [
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'a' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.2 } } },
              { '@type': 'Equals', left: { '@type': 'DataValue', variable: 'b' }, right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': 0.1 } } },
              {
                '@type': 'Eval',
                expression: {
                  '@type': 'Divide',
                  left: { '@type': 'ArithmeticValue', variable: 'a' },
                  right: { '@type': 'ArithmeticValue', variable: 'b' },
                },
                result: { '@type': 'ArithmeticValue', variable: 'result' },
              },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'i' }, list: range2000 },
              { '@type': 'Member', member: { '@type': 'Value', variable: 'j' }, list: range2000 },
            ],
          },
        },
      }
      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      expect(result.bindings.length).to.equal(1)
      // GroupBy with group_by: ['none'] evaluates all 4M solutions, Select filters to just result
      // Select with variables: [''] filters everything, returns empty binding
      expect(Object.keys(result.bindings[0]).length).to.equal(0)
    }).timeout(60000)
  })
})
