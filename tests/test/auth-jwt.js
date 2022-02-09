const { expect } = require('chai')
const { Agent, db, endpoint } = require('../lib')

describe('auth-jwt', function () {
  let agent

  before(function () {
    agent = new Agent()
    agent.set('Authorization', `Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RrZXkifQ.eyJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSNhZ2VudF9uYW1lIjoiYWRtaW4iLCJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSN1c2VyX2lkZW50aWZpZXIiOiJhZG1pbkB1c2VyLmNvbSIsImlzcyI6Imh0dHBzOi8vdGVybWludXNodW0000000000aDAuY29tLyIsInN1YiI6ImF1dGgwfDVlZmVmY2NkYmIzMzEzMDAxMzlkNTAzNyIsImF1ZCI6WyJodHRwczovL3Rlcm1pbnVzaHViL3JlZ2lzdGVyVXNlciIsImh0dHBzOi8vdGVybWludXNodWIuZXUuYXV0aDAuY29tL3VzZXJpbmZvIl0sImlhdCI6MTU5Mzc2OTE4MywiYXpwIjoiTUpKbmRHcDB6VWRNN28zUE9UUVBtUkpJbVkyaG8wYWkiLCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGVtYWlsIn0.hxJphuKyWLbTLTgFq37tHQvNaxDwWxeOyDVbEemYoWDhBbSbjcjP034jJ0PhupYqtdadZV4un4j9QkJeYDLNtZLD7q4tErNK5bDw9gM1z9335BSu9htjLOZEF2_DqJYLGdbazWA3MAGkg6corOCXDVyBZpToekvylsGAMztkZaeAIgnsJlHxIIMMLQHrHNCBRPC1U6ZJQc7WZdgB-gefVlVQco0w8_Q0Z28DeshD9y3XChTMeTAAT-URwmz61RB6aUFMXpr4tTtYwyXGsWdu46LuDNxOV070eTybthDpDjyYSDsn-i4YbHvDGN5kUen9pw6b47CkSdhsSSjVQLsNkA`)
  })

  it('fails connect on non existing user', async function () {
    const r = await agent.get('/api/')
    expect(r.status).to.equal(401)
  })

  it('fails db create on non existing user', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await db.create(agent, path)
    expect(r.status).to.equal(401)
  })
})
