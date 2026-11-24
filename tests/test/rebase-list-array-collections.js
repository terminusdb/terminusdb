const { expect } = require('chai')
const { Agent } = require('../lib')

// Rebase of commits that add, remove or replace elements of a List or Array
// subdocument collection must merge like Set does: 200 with the merged
// document, or 409 with a witness on a real conflict. It must never return
// 200 with a corrupted document or 400 for a valid merge.
describe('rebase-list-array-collections', function () {
  let agent
  let dbName

  before(async function () {
    agent = new Agent().auth()
    dbName = agent.dbName
    await agent.post('/api/db/admin/' + dbName).send({ label: 'rebase list array' }).ok(res => true)
  })

  const variant = (sku, price) => ({
    '@type': 'Variant',
    sku,
    price,
  })

  const schemaFor = (container) => [
    {
      '@type': 'Class',
      '@id': 'Product',
      '@key': { '@type': 'Lexical', '@fields': ['name'] },
      '@base': 'Product/',
      name: 'xsd:string',
      title: 'xsd:string',
      variants: { '@type': container, '@class': 'Variant' },
    },
    {
      '@type': 'Class',
      '@id': 'Variant',
      '@subdocument': [],
      '@key': { '@type': 'Lexical', '@fields': ['sku'] },
      '@base': 'Variant/',
      sku: 'xsd:string',
      price: 'xsd:integer',
    },
  ]

  const baseDoc = () => ({
    '@type': 'Product',
    name: 'tee',
    title: 'Tee',
    variants: [variant('B', 20), variant('R', 20)],
  })

  const request = (promise) => promise.ok(res => true)

  const skus = (doc) => {
    const variants = doc.body.variants
    expect(Array.isArray(variants), 'variants is an array').to.equal(true)
    return variants.map(v => {
      expect(v).to.be.an('object')
      return v.sku
    })
  }

  const setup = async (container, scenario) => {
    const suffix = `${container.toLowerCase()}-${scenario}-${Math.random().toString(36).slice(2, 8)}`
    // fresh database per case keeps branches isolated
    const local = dbName + '-' + suffix
    await request(agent.post('/api/db/admin/' + local).send({ label: suffix }))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=schema&graph_type=schema`).send(schemaFor(container)))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=base`).send(baseDoc()))
    await request(agent.post(`/api/branch/admin/${local}/local/branch/other`).send({
      origin: `admin/${local}/local/branch/main`,
    }))

    let otherVariants
    if (scenario === 'add') {
      otherVariants = ['B', 'R', 'A']
    } else if (scenario === 'remove') {
      otherVariants = ['B']
    } else if (scenario === 'replace') {
      otherVariants = ['B', 'A']
    } else {
      otherVariants = ['B', 'R']
    }
    const otherDoc = {
      '@type': 'Product',
      name: 'tee',
      title: 'Tee',
      variants: otherVariants.map(sku => variant(sku, sku === 'B' ? 20 : sku === 'R' ? 20 : 30)),
    }
    await request(agent.put(`/api/document/admin/${local}/local/branch/other?author=a&message=other`).send(otherDoc))

    // main edits title and price of B; other only touched variants
    const mainDoc = {
      '@type': 'Product',
      name: 'tee',
      title: 'Tee v2',
      variants: [variant('B', 21), variant('R', 20)],
    }
    await request(agent.put(`/api/document/admin/${local}/local/branch/main?author=a&message=main`).send(mainDoc))

    const rebase = await request(agent.post(`/api/rebase/admin/${local}/local/branch/other`).send({
      rebase_from: `admin/${local}/local/branch/main`,
      author: 'a',
    }))
    const doc = await request(agent.get(`/api/document/admin/${local}/local/branch/other?id=Product/tee`))
    return { rebase, doc }
  }

  for (const container of ['Set', 'List', 'Array']) {
    for (const scenario of ['add', 'remove', 'replace', 'control']) {
      it(`${container} ${scenario}`, async function () {
        this.timeout(60000)
        const { rebase, doc } = await setup(container, scenario)
        expect(rebase.status, `rebase status: ${JSON.stringify(rebase.body)}`).to.equal(200)
        expect(doc.status).to.equal(200)
        const got = skus(doc)
        if (container === 'List' || container === 'Array') {
          // order matters for List/Array
          const expected = scenario === 'add'
            ? ['B', 'R', 'A']
            : scenario === 'remove'
              ? ['B']
              : scenario === 'replace'
                ? ['B', 'A']
                : ['B', 'R']
          expect(got).to.deep.equal(expected)
        } else {
          const expectedSkus = scenario === 'add'
            ? ['A', 'B', 'R']
            : scenario === 'remove'
              ? ['B']
              : scenario === 'replace'
                ? ['A', 'B']
                : ['B', 'R']
          expect(got.slice().sort()).to.deep.equal(expectedSkus.slice().sort())
        }
        // main's title and B price edits survive the merge
        expect(doc.body.title).to.equal('Tee v2')
        const b = doc.body.variants.find(v => v.sku === 'B')
        expect(b.price).to.equal(21)
        // no duplicated or dangling elements
        expect(new Set(got).size).to.equal(got.length)
        if (container === 'Array') {
          doc.body.variants.forEach((v) => {
            expect(v).to.be.an('object')
            expect(v.sku).to.be.a('string')
          })
        }
      })
    }
  }

  // Both branches edit the same element: a real conflict must be a 409 with a
  // witness, never a 200 with a corrupted document or a 400 schema error.
  for (const container of ['List', 'Array']) {
    it(`${container} conflict`, async function () {
      this.timeout(60000)
      const suffix = `${container.toLowerCase()}-conflict-${Math.random().toString(36).slice(2, 8)}`
      const local = dbName + '-' + suffix
      await request(agent.post('/api/db/admin/' + local).send({ label: suffix }))
      await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=schema&graph_type=schema`).send(schemaFor(container)))
      await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=base`).send(baseDoc()))
      await request(agent.post(`/api/branch/admin/${local}/local/branch/other`).send({
        origin: `admin/${local}/local/branch/main`,
      }))
      const docWithB = price => ({
        '@type': 'Product',
        name: 'tee',
        title: 'Tee',
        variants: [variant('B', price), variant('R', 20)],
      })
      await request(agent.put(`/api/document/admin/${local}/local/branch/other?author=a&message=other`).send(docWithB(22)))
      await request(agent.put(`/api/document/admin/${local}/local/branch/main?author=a&message=main`).send(docWithB(21)))
      const rebase = await request(agent.post(`/api/rebase/admin/${local}/local/branch/other`).send({
        rebase_from: `admin/${local}/local/branch/main`,
        author: 'a',
      }))
      expect(rebase.status, `rebase status: ${JSON.stringify(rebase.body)}`).to.equal(409)
      expect(rebase.body['api:error']['@type']).to.equal('api:RebaseMergeConflict')
      expect(rebase.body['api:error']['api:document']).to.equal('Product/tee')
    })
  }

  // A document can carry a List and a Set at once. When the replayed commit
  // touches List cells the whole document merges three-way, and an element
  // ours removed from the Set must stay removed even though theirs added
  // others to it.
  it('mixed List and Set: ours removes a Set element, theirs adds one', async function () {
    this.timeout(60000)
    const suffix = `mixed-${Math.random().toString(36).slice(2, 8)}`
    const local = dbName + '-' + suffix
    const mixedSchema = [
      {
        '@type': 'Class',
        '@id': 'Product',
        '@key': { '@type': 'Lexical', '@fields': ['name'] },
        '@base': 'Product/',
        name: 'xsd:string',
        variants: { '@type': 'List', '@class': 'Variant' },
        tags: { '@type': 'Set', '@class': 'Variant' },
      },
      schemaFor('List')[1],
    ]
    await request(agent.post('/api/db/admin/' + local).send({ label: suffix }))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=schema&graph_type=schema`).send(mixedSchema))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=base`).send({
      '@type': 'Product',
      name: 'tee',
      variants: [variant('B', 20), variant('R', 20)],
      tags: [variant('T1', 1), variant('T2', 2)],
    }))
    await request(agent.post(`/api/branch/admin/${local}/local/branch/other`).send({
      origin: `admin/${local}/local/branch/main`,
    }))
    await request(agent.put(`/api/document/admin/${local}/local/branch/other?author=a&message=other`).send({
      '@type': 'Product',
      name: 'tee',
      variants: [variant('A', 30), variant('B', 20), variant('R', 20)],
      tags: [variant('T1', 1), variant('T2', 2), variant('T3', 3)],
    }))
    await request(agent.put(`/api/document/admin/${local}/local/branch/main?author=a&message=main`).send({
      '@type': 'Product',
      name: 'tee',
      variants: [variant('B', 20), variant('R', 20)],
      tags: [variant('T1', 1)],
    }))
    const rebase = await request(agent.post(`/api/rebase/admin/${local}/local/branch/other`).send({
      rebase_from: `admin/${local}/local/branch/main`,
      author: 'a',
    }))
    expect(rebase.status, `rebase status: ${JSON.stringify(rebase.body)}`).to.equal(200)
    const doc = await request(agent.get(`/api/document/admin/${local}/local/branch/other?id=Product/tee`))
    expect(doc.status).to.equal(200)
    expect(skus(doc)).to.deep.equal(['A', 'B', 'R'])
    const tagSkus = doc.body.tags.map(t => t.sku).sort()
    expect(tagSkus).to.deep.equal(['T1', 'T3'])
  })

  // Lists can hold embedded @subdocument elements next to @shared and plain
  // linked documents, which travel as id strings. A three-way merge must keep
  // those references intact: same ids, same order, no duplication.
  it('List of @subdocument, @shared and linked elements', async function () {
    this.timeout(60000)
    const suffix = `linked-${Math.random().toString(36).slice(2, 8)}`
    const local = dbName + '-' + suffix
    const linkedSchema = [
      {
        '@type': 'Class',
        '@id': 'Maker',
        '@key': { '@type': 'Lexical', '@fields': ['name'] },
        '@base': 'Maker/',
        name: 'xsd:string',
      },
      {
        '@type': 'Class',
        '@id': 'SharedPart',
        '@shared': [],
        '@key': { '@type': 'Lexical', '@fields': ['sku'] },
        '@base': 'SharedPart/',
        sku: 'xsd:string',
        price: 'xsd:integer',
      },
      schemaFor('List')[1],
      {
        '@type': 'Class',
        '@id': 'Product',
        '@key': { '@type': 'Lexical', '@fields': ['name'] },
        '@base': 'Product/',
        name: 'xsd:string',
        title: 'xsd:string',
        variants: { '@type': 'List', '@class': 'Variant' },
        parts: { '@type': 'List', '@class': 'SharedPart' },
        makers: { '@type': 'List', '@class': 'Maker' },
      },
    ]
    const baseLinked = {
      '@type': 'Product',
      name: 'tee',
      title: 'Tee',
      variants: [variant('B', 20), variant('R', 20)],
      parts: ['SharedPart/P1', 'SharedPart/P2'],
      makers: ['Maker/Acme'],
    }
    const sharedPart = (sku, price) => ({ '@type': 'SharedPart', sku, price })
    await request(agent.post('/api/db/admin/' + local).send({ label: suffix }))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=schema&graph_type=schema`).send(linkedSchema))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=base`).send([
      { '@type': 'Maker', name: 'Acme' },
      sharedPart('P1', 10),
      sharedPart('P2', 20),
      baseLinked,
    ]))
    await request(agent.post(`/api/branch/admin/${local}/local/branch/other`).send({
      origin: `admin/${local}/local/branch/main`,
    }))
    // theirs adds one element to every list, plus the referenced documents
    await request(agent.post(`/api/document/admin/${local}/local/branch/other?author=a&message=refs`).send([
      { '@type': 'Maker', name: 'Globex' },
      sharedPart('P3', 30),
    ]))
    await request(agent.put(`/api/document/admin/${local}/local/branch/other?author=a&message=other`).send({
      ...baseLinked,
      variants: [variant('A', 30), variant('B', 20), variant('R', 20)],
      parts: ['SharedPart/P1', 'SharedPart/P2', 'SharedPart/P3'],
      makers: ['Maker/Acme', 'Maker/Globex'],
    }))
    // ours only edits title and the price of B
    await request(agent.put(`/api/document/admin/${local}/local/branch/main?author=a&message=main`).send({
      ...baseLinked,
      title: 'Tee v2',
      variants: [variant('B', 21), variant('R', 20)],
    }))
    const rebase = await request(agent.post(`/api/rebase/admin/${local}/local/branch/other`).send({
      rebase_from: `admin/${local}/local/branch/main`,
      author: 'a',
    }))
    expect(rebase.status, `rebase status: ${JSON.stringify(rebase.body)}`).to.equal(200)
    const doc = await request(agent.get(`/api/document/admin/${local}/local/branch/other?id=Product/tee`))
    expect(doc.status).to.equal(200)
    expect(skus(doc)).to.deep.equal(['A', 'B', 'R'])
    expect(doc.body.variants.find(v => v.sku === 'B').price).to.equal(21)
    // linked and shared elements survive as plain, ordered id strings
    expect(doc.body.parts).to.deep.equal(['SharedPart/P1', 'SharedPart/P2', 'SharedPart/P3'])
    expect(doc.body.makers).to.deep.equal(['Maker/Acme', 'Maker/Globex'])
    expect(doc.body.title).to.equal('Tee v2')
  })

  // @unfoldable documents embed on read but are separate documents with
  // stable ids. The merge must treat them as references — order and
  // membership by id — while their content edits replay with the commit.
  // Random-key unfoldable documents have no key fields to fall back on, so
  // a lost @id used to make the rebase fail outright.
  it('List of @unfoldable elements with Lexical and Random keys', async function () {
    this.timeout(60000)
    const suffix = `unfoldable-${Math.random().toString(36).slice(2, 8)}`
    const local = dbName + '-' + suffix
    const unfoldableSchema = [
      {
        '@type': 'Class',
        '@id': 'Gadget',
        '@unfoldable': [],
        '@key': { '@type': 'Lexical', '@fields': ['sku'] },
        '@base': 'Gadget/',
        sku: 'xsd:string',
        price: 'xsd:integer',
      },
      {
        '@type': 'Class',
        '@id': 'Doohickey',
        '@unfoldable': [],
        '@key': { '@type': 'Random' },
        '@base': 'Doohickey/',
        note: 'xsd:string',
      },
      schemaFor('List')[1],
      {
        '@type': 'Class',
        '@id': 'Product',
        '@key': { '@type': 'Lexical', '@fields': ['name'] },
        '@base': 'Product/',
        name: 'xsd:string',
        title: 'xsd:string',
        variants: { '@type': 'List', '@class': 'Variant' },
        gadgets: { '@type': 'List', '@class': 'Gadget' },
        doohickeys: { '@type': 'List', '@class': 'Doohickey' },
      },
    ]
    const gadget = (sku, price) => ({ '@type': 'Gadget', sku, price })
    const doohickey = (id, note) => ({ '@type': 'Doohickey', '@id': id, note })
    const baseUnfoldable = {
      '@type': 'Product',
      name: 'tee',
      title: 'Tee',
      variants: [variant('B', 20), variant('R', 20)],
      gadgets: ['Gadget/G1', 'Gadget/G2'],
      doohickeys: ['Doohickey/D1', 'Doohickey/D2'],
    }
    await request(agent.post('/api/db/admin/' + local).send({ label: suffix }))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=schema&graph_type=schema`).send(unfoldableSchema))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=base`).send([
      gadget('G1', 5),
      gadget('G2', 7),
      doohickey('Doohickey/D1', 'alpha'),
      doohickey('Doohickey/D2', 'beta'),
      baseUnfoldable,
    ]))
    await request(agent.post(`/api/branch/admin/${local}/local/branch/other`).send({
      origin: `admin/${local}/local/branch/main`,
    }))
    // theirs adds referenced documents, edits their content, drops G2 from
    // the gadgets list and prepends a variant
    await request(agent.post(`/api/document/admin/${local}/local/branch/other?author=a&message=refs`).send([
      gadget('G3', 9),
      doohickey('Doohickey/D3', 'gamma'),
    ]))
    await request(agent.put(`/api/document/admin/${local}/local/branch/other?author=a&message=other`).send([
      gadget('G1', 6),
      doohickey('Doohickey/D1', 'alpha2'),
      {
        ...baseUnfoldable,
        variants: [variant('A', 30), variant('B', 20), variant('R', 20)],
        gadgets: ['Gadget/G1', 'Gadget/G3'],
        doohickeys: ['Doohickey/D1', 'Doohickey/D2', 'Doohickey/D3'],
      },
    ]))
    // ours edits title and the price of R
    await request(agent.put(`/api/document/admin/${local}/local/branch/main?author=a&message=main`).send({
      ...baseUnfoldable,
      title: 'Tee v2',
      variants: [variant('B', 20), variant('R', 22)],
    }))
    const rebase = await request(agent.post(`/api/rebase/admin/${local}/local/branch/other`).send({
      rebase_from: `admin/${local}/local/branch/main`,
      author: 'a',
    }))
    expect(rebase.status, `rebase status: ${JSON.stringify(rebase.body)}`).to.equal(200)
    const doc = await request(agent.get(`/api/document/admin/${local}/local/branch/other?id=Product/tee`))
    expect(doc.status).to.equal(200)
    expect(skus(doc)).to.deep.equal(['A', 'B', 'R'])
    expect(doc.body.variants.find(v => v.sku === 'R').price).to.equal(22)
    // unfoldable elements read back embedded; identity and order hold
    expect(doc.body.gadgets.map(g => g['@id'])).to.deep.equal(['Gadget/G1', 'Gadget/G3'])
    expect(doc.body.doohickeys.map(d => d['@id'])).to.deep.equal(['Doohickey/D1', 'Doohickey/D2', 'Doohickey/D3'])
    // content edits to the referenced documents survive the rebase
    expect(doc.body.gadgets.find(g => g.sku === 'G1').price).to.equal(6)
    expect(doc.body.doohickeys.find(d => d['@id'] === 'Doohickey/D1').note).to.equal('alpha2')
    expect(doc.body.title).to.equal('Tee v2')
    // dropping an element from the list does not delete the document
    const dropped = await request(agent.get(`/api/document/admin/${local}/local/branch/other?id=Gadget/G2`))
    expect(dropped.status).to.equal(200)
    expect(dropped.body.price).to.equal(7)
  })
})
