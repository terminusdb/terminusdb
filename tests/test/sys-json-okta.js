const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('sys:JSON Bug - Okta Payload Example (Multiple updates)', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('Multiple updates with JSON payloads', function () {
    let userId

    before(async function () {
      await db.create(agent, { label: 'Test Okta', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'User',
          '@type': 'Class',
          '@key': {
            '@fields': ['userId'],
            '@type': 'Lexical',
          },
          userId: 'xsd:string',
          oktaData: 'sys:JSON',
        },
      })

      // Create initial user document
      const result = await document.insert(agent, {
        instance: {
          '@type': 'User',
          userId: 'user123',
          oktaData: {
            id: '00u123abc',
            status: 'ACTIVE',
            created: '2023-01-01T00:00:00.000Z',
            profile: {
              login: 'user@example.com',
              email: 'user@example.com',
              firstName: 'John',
              lastName: 'Doe',
            },
            credentials: {
              provider: {
                type: 'OKTA',
                name: 'OKTA',
              },
            },
          },
        },
      })

      userId = result.body[0]
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should allow first update of Okta payload', async function () {
      const result = await document.replace(agent, {
        instance: {
          '@id': userId,
          '@type': 'User',
          userId: 'user123',
          oktaData: {
            id: '00u123abc',
            status: 'PROVISIONED', // Changed
            created: '2023-01-01T00:00:00.000Z',
            lastUpdated: '2023-01-02T00:00:00.000Z', // New field
            profile: {
              login: 'user@example.com',
              email: 'user@example.com',
              firstName: 'John',
              lastName: 'Doe',
            },
            credentials: {
              provider: {
                type: 'OKTA',
                name: 'OKTA',
              },
            },
          },
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Update failed: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
    })

    it('should allow second update of Okta payload', async function () {
      const result = await document.replace(agent, {
        instance: {
          '@id': userId,
          '@type': 'User',
          userId: 'user123',
          oktaData: {
            id: '00u123abc',
            status: 'ACTIVE', // Changed back
            created: '2023-01-01T00:00:00.000Z',
            lastUpdated: '2023-01-03T00:00:00.000Z', // Updated
            profile: {
              login: 'user@example.com',
              email: 'user@example.com',
              firstName: 'Jane', // Changed
              lastName: 'Doe',
            },
            credentials: {
              provider: {
                type: 'OKTA',
                name: 'OKTA',
              },
            },
          },
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Update failed: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
    })

    it('should allow multiple rapid updates (10 iterations)', async function () {
      for (let i = 0; i < 10; i++) {
        const result = await document.replace(agent, {
          instance: {
            '@id': userId,
            '@type': 'User',
            userId: 'user123',
            oktaData: {
              id: '00u123abc',
              status: i % 2 === 0 ? 'ACTIVE' : 'PROVISIONED',
              created: '2023-01-01T00:00:00.000Z',
              lastUpdated: `2023-01-${String(i + 4).padStart(2, '0')}T00:00:00.000Z`,
              syncNumber: i,
              profile: {
                login: 'user@example.com',
                email: 'user@example.com',
                firstName: 'John',
                lastName: `Doe${i}`,
              },
              credentials: {
                provider: {
                  type: 'OKTA',
                  name: 'OKTA',
                },
              },
            },
          },
        }).unverified()

        if (result.status !== 200) {
          throw new Error(`Update ${i + 1} failed: ${JSON.stringify(result.body)}`)
        }

        expect(result.status).to.equal(200)
      }

      // Verify final state
      const user = await document.get(agent, { query: { id: userId } })
      expect(user.body.oktaData.syncNumber).to.equal(9)
    })
  })
})
