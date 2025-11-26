const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('JSON typecast workflow tests', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('xdd:json type behavior', function () {
    it('should report xdd:json as type for typecast result', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'json_str' },
            right: {
              '@type': 'Value',
              data: {
                '@type': 'xsd:string',
                '@value': '{"name":"Alice","age":30}'
              }
            }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json_str' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/xdd#json'
            },
            result: { '@type': 'Value', variable: 'json_dict' }
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'Value', variable: 'json_dict' },
            type: { '@type': 'Value', variable: 'json_type' }
          }
        ]
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].json_type).to.equal('xdd:json')
    })

    it('should allow dot operator on xdd:json after typecast to sys:Dictionary', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'json_str' },
            right: {
              '@type': 'Value',
              data: {
                '@type': 'xsd:string',
                '@value': '{"name":"Bob","age":25}'
              }
            }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json_str' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/xdd#json'
            },
            result: { '@type': 'Value', variable: 'json_typed' }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json_typed' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/sys#Dictionary'
            },
            result: { '@type': 'Value', variable: 'json_dict' }
          },
          {
            '@type': 'Dot',
            document: { '@type': 'Value', variable: 'json_dict' },
            field: {
              '@type': 'Value',
              data: { '@type': 'xsd:string', '@value': 'name' }
            },
            value: { '@type': 'Value', variable: 'extracted_name' }
          }
        ]
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].extracted_name).to.equal('Bob')
    })
  })

  describe('xdd:json to sys:Dictionary typecast', function () {
    it('should convert xdd:json to sys:Dictionary via typecast', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'json_str' },
            right: {
              '@type': 'Value',
              data: {
                '@type': 'xsd:string',
                '@value': '{"name":"Charlie","age":35}'
              }
            }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json_str' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/xdd#json'
            },
            result: { '@type': 'Value', variable: 'json_dict' }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json_dict' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/sys#Dictionary'
            },
            result: { '@type': 'Value', variable: 'plain_dict' }
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'Value', variable: 'plain_dict' },
            type: { '@type': 'Value', variable: 'dict_type' }
          }
        ]
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      // Should report a type for plain dict
      expect(r.body.bindings[0].dict_type).to.exist
    })

    it('should allow dot operator on sys:Dictionary after typecast from xdd:json', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'json_str' },
            right: {
              '@type': 'Value',
              data: {
                '@type': 'xsd:string',
                '@value': '{"name":"Dave","age":40}'
              }
            }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json_str' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/xdd#json'
            },
            result: { '@type': 'Value', variable: 'json_dict' }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json_dict' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/sys#Dictionary'
            },
            result: { '@type': 'Value', variable: 'plain_dict' }
          },
          {
            '@type': 'Dot',
            document: { '@type': 'Value', variable: 'plain_dict' },
            field: {
              '@type': 'Value',
              data: { '@type': 'xsd:string', '@value': 'name' }
            },
            value: { '@type': 'Value', variable: 'extracted_name' }
          }
        ]
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].extracted_name).to.equal('Dave')
    })
  })

  describe('sys:Dictionary to xdd:json typecast', function () {
    it('should convert sys:Dictionary to xdd:json via typecast', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'doc' },
            right: {
              '@type': 'Value',
              dictionary: {
                '@type': 'DictionaryTemplate',
                data: [
                  {
                    '@type': 'FieldValuePair',
                    field: 'name',
                    value: {
                      '@type': 'Value',
                      data: {
                        '@type': 'xsd:string',
                        '@value': 'Eve'
                      }
                    }
                  },
                  {
                    '@type': 'FieldValuePair',
                    field: 'age',
                    value: {
                      '@type': 'Value',
                      data: {
                        '@type': 'xsd:integer',
                        '@value': 45
                      }
                    }
                  }
                ]
              }
            }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'doc' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/xdd#json'
            },
            result: { '@type': 'Value', variable: 'json_value' }
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'Value', variable: 'json_value' },
            type: { '@type': 'Value', variable: 'json_type' }
          }
        ]
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].json_type).to.equal('xdd:json')
    })

    it('should convert xdd:json back to string via typecast', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'original_str' },
            right: {
              '@type': 'Value',
              data: {
                '@type': 'xsd:string',
                '@value': '{"name":"Frank","age":50}'
              }
            }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'original_str' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/xdd#json'
            },
            result: { '@type': 'Value', variable: 'json_dict' }
          },
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json_dict' },
            type: {
              '@type': 'NodeValue',
              node: 'http://www.w3.org/2001/XMLSchema#string'
            },
            result: { '@type': 'Value', variable: 'back_to_str' }
          }
        ]
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      // Should be able to round-trip (though formatting may differ)
      expect(r.body.bindings[0].back_to_str['@value']).to.be.a('string')
    })
  })

  describe('sys:Dictionary type behavior', function () {
    it('should report sys:Dictionary type for DictionaryTemplate', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'doc' },
            right: {
              '@type': 'Value',
              dictionary: {
                '@type': 'DictionaryTemplate',
                data: [
                  {
                    '@type': 'FieldValuePair',
                    field: 'name',
                    value: {
                      '@type': 'Value',
                      data: {
                        '@type': 'xsd:string',
                        '@value': 'George'
                      }
                    }
                  }
                ]
              }
            }
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'Value', variable: 'doc' },
            type: { '@type': 'Value', variable: 'doc_type' }
          }
        ]
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      // Should report sys:Dictionary type
      expect(r.body.bindings[0].doc_type).to.equal('sys:Dictionary')
    })

    it('should allow dot operator on DictionaryTemplate', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'doc' },
            right: {
              '@type': 'Value',
              dictionary: {
                '@type': 'DictionaryTemplate',
                data: [
                  {
                    '@type': 'FieldValuePair',
                    field: 'name',
                    value: {
                      '@type': 'Value',
                      data: {
                        '@type': 'xsd:string',
                        '@value': 'Hannah'
                      }
                    }
                  },
                  {
                    '@type': 'FieldValuePair',
                    field: 'age',
                    value: {
                      '@type': 'Value',
                      data: {
                        '@type': 'xsd:integer',
                        '@value': 55
                      }
                    }
                  }
                ]
              }
            }
          },
          {
            '@type': 'Dot',
            document: { '@type': 'Value', variable: 'doc' },
            field: {
              '@type': 'Value',
              data: { '@type': 'xsd:string', '@value': 'name' }
            },
            value: { '@type': 'Value', variable: 'extracted_name' }
          }
        ]
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].extracted_name).to.equal('Hannah')
    })
  })

  describe('Complete workflow: string → xdd:json → dict → xdd:json → string', function () {
    it('should support full round-trip conversion', async function () {
      const query = {
        '@type': 'And',
        and: [
          // Start with string
          {
            '@type': 'Equals',
            left: { '@type': 'Value', variable: 'str1' },
            right: {
              '@type': 'Value',
              data: {
                '@type': 'xsd:string',
                '@value': '{"name":"Ian","age":60}'
              }
            }
          },
          // string → xdd:json
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'str1' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/xdd#json'
            },
            result: { '@type': 'Value', variable: 'json1' }
          },
          // xdd:json → dict
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json1' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/sys#Dictionary'
            },
            result: { '@type': 'Value', variable: 'dict' }
          },
          // Extract field from dict
          {
            '@type': 'Dot',
            document: { '@type': 'Value', variable: 'dict' },
            field: {
              '@type': 'Value',
              data: { '@type': 'xsd:string', '@value': 'name' }
            },
            value: { '@type': 'Value', variable: 'name' }
          },
          // dict → xdd:json
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'dict' },
            type: {
              '@type': 'NodeValue',
              node: 'http://terminusdb.com/schema/xdd#json'
            },
            result: { '@type': 'Value', variable: 'json2' }
          },
          // xdd:json → string
          {
            '@type': 'Typecast',
            value: { '@type': 'Value', variable: 'json2' },
            type: {
              '@type': 'NodeValue',
              node: 'http://www.w3.org/2001/XMLSchema#string'
            },
            result: { '@type': 'Value', variable: 'str2' }
          }
        ]
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(r.body.bindings[0].name).to.equal('Ian')
      expect(r.body.bindings[0].str2['@value']).to.be.a('string')
    })
  })
})
