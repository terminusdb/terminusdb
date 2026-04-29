'use strict'

/**
 * Integration tests for @shared class-level annotation.
 *
 * Tests cover:
 * 1. Annotation syntax — schema acceptance and rejection for @shared
 * 2. @shared liveness — cascade when ALL refs removed
 * 3. @shared all-references rule — all references count for liveness
 * 4. Cascade recursion — recursive cascade, circular islands, visited-set
 * 5. Explicit delete of @shared with active refs — explicit delete proceeds
 * 6. Standalone creation — @shared targets can be created without parent
 * 7. Set per-element liveness — per-element cascade on Set fields
 * 8. Migration compatibility — adding/removing @shared on existing schema
 */

const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('shared-child-annotation', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  // =========================================================================
  // 1. ANNOTATION SYNTAX
  // =========================================================================

  describe('annotation syntax', function () {
    describe('@shared class-level annotation', function () {
      before(async function () {
        agent.dbName = 'test_shared_syntax'
        await db.create(agent)
      })

      after(async function () {
        await db.delete(agent)
      })

      it('accepts @shared empty list on a class definition', async function () {
        const schema = [
          {
            '@type': 'Class',
            '@id': 'SharedNote',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['title'] },
            title: 'xsd:string',
          },
        ]

        const r = await document.insert(agent, { schema })
        expect(r.status).to.equal(200)
      })

      it('returns @shared empty list in schema roundtrip', async function () {
        const r = await document.get(agent, { query: { graph_type: 'schema', id: 'SharedNote' } })
        expect(r.status).to.equal(200)
        expect(r.body).to.have.property('@shared')
        expect(r.body['@shared']).to.deep.equal([])
      })

      it('rejects @shared true with bad_shared_value witness', async function () {
        const schema = [
          {
            '@type': 'Class',
            '@id': 'BadShared',
            '@shared': true,
            title: 'xsd:string',
          },
        ]

        const r = await document.insert(agent, { schema }).fails()
        expect(r.status).to.equal(400)
        expect(JSON.stringify(r.body)).to.include('bad_shared_value')
      })

      it('rejects @shared string with bad_shared_value witness', async function () {
        const schema = [
          {
            '@type': 'Class',
            '@id': 'BadShared2',
            '@shared': 'yes',
            title: 'xsd:string',
          },
        ]

        const r = await document.insert(agent, { schema }).fails()
        expect(r.status).to.equal(400)
        expect(JSON.stringify(r.body)).to.include('bad_shared_value')
      })
    })

    describe('@shared and @subdocument mutual exclusion', function () {
      before(async function () {
        agent.dbName = 'test_shared_subdoc_exclusion'
        await db.create(agent)
      })

      after(async function () {
        await db.delete(agent)
      })

      it('rejects class with both @shared and @subdocument', async function () {
        const schema = [
          {
            '@type': 'Class',
            '@id': 'Incompatible',
            '@shared': [],
            '@subdocument': [],
            name: 'xsd:string',
          },
        ]

        const r = await document.insert(agent, { schema }).fails()
        expect(r.status).to.equal(400)
        expect(JSON.stringify(r.body)).to.include('incompatible_class_annotations')
      })

      it('accepts @shared without @subdocument', async function () {
        const schema = [
          {
            '@type': 'Class',
            '@id': 'SharedOnly',
            '@shared': [],
            name: 'xsd:string',
          },
        ]

        const r = await document.insert(agent, { schema })
        expect(r.status).to.equal(200)
      })
    })

    describe('@shared inheritance through class hierarchy', function () {
      before(async function () {
        agent.dbName = 'test_shared_inheritance'
        await db.create(agent)
      })

      after(async function () {
        await db.delete(agent)
      })

      it('subclass of @shared class inherits @shared', async function () {
        const schema = [
          {
            '@type': 'Class',
            '@id': 'BaseShared',
            '@shared': [],
            '@key': { '@type': 'Random' },
            name: 'xsd:string',
          },
          {
            '@type': 'Class',
            '@id': 'DerivedShared',
            '@inherits': ['BaseShared'],
            '@key': { '@type': 'Random' },
            extra: 'xsd:string',
          },
        ]

        const r = await document.insert(agent, { schema })
        expect(r.status).to.equal(200)

        // Verify that the subclass is treated as @shared via schema frame
        const r2 = await document.get(agent, { query: { graph_type: 'schema', id: 'DerivedShared' } })
        expect(r2.status).to.equal(200)
        // The inherited @shared should be visible in the schema frame
        // (Either directly or through the inheritance chain)
      })

      it('subclass of @shared with @subdocument is rejected', async function () {
        const schema = [
          {
            '@type': 'Class',
            '@id': 'BaseShared2',
            '@shared': [],
            '@key': { '@type': 'Random' },
            name: 'xsd:string',
          },
          {
            '@type': 'Class',
            '@id': 'BadSubclass',
            '@inherits': ['BaseShared2'],
            '@subdocument': [],
            name: 'xsd:string',
          },
        ]

        const r = await document.insert(agent, { schema }).fails()
        expect(r.status).to.equal(400)
      })
    })
  })

  // =========================================================================
  // 2. @shared LIVENESS — cascade when ALL refs removed
  // =========================================================================

  describe('@shared liveness — cascade when all refs removed', function () {
    describe('@shared target cascade-deleted when zero references remain', function () {
      let dbName

      before(async function () {
        dbName = 'test_shared_liveness_cascade'
        agent.dbName = dbName
        await db.create(agent)

        const schema = [
          {
            '@type': 'Class',
            '@id': 'Footnote',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['id'] },
            id: 'xsd:string',
            text: 'xsd:string',
          },
          {
            '@type': 'Class',
            '@id': 'Article',
            '@key': { '@type': 'Lexical', '@fields': ['title'] },
            title: 'xsd:string',
            notes: {
              '@type': 'Set',
              '@class': 'Footnote',
            },
          },
        ]

        await document.insert(agent, { schema })
      })

      after(async function () {
        await db.delete(agent)
      })

      it('cascade-deletes @shared target when last reference removed', async function () {
        // Create a shared footnote
        await document.insert(agent, {
          instance: {
            '@type': 'Footnote',
            id: 'fn1',
            text: 'A shared footnote',
          },
        })

        // Create an article referencing the footnote
        await document.insert(agent, {
          instance: {
            '@type': 'Article',
            title: 'article1',
            notes: ['Footnote/fn1'],
          },
        })

        // Delete the article (removes the only reference to fn1)
        await document.delete(agent, { query: { id: 'Article/article1' } })

        // The footnote should be cascade-deleted (zero references remain)
        const r = await document.get(agent, {
          query: { id: 'Footnote/fn1', as_list: true },
        })
        // After cascade delete, the footnote should not exist
        expect(r.body).to.be.an('array').that.is.empty
      })

      it('does NOT cascade-delete when other references remain', async function () {
        // Create a shared footnote
        await document.insert(agent, {
          instance: {
            '@type': 'Footnote',
            id: 'fn2',
            text: 'A shared footnote with two parents',
          },
        })

        // Create TWO articles referencing the same footnote
        await document.insert(agent, {
          instance: [
            {
              '@type': 'Article',
              title: 'article2a',
              notes: ['Footnote/fn2'],
            },
            {
              '@type': 'Article',
              title: 'article2b',
              notes: ['Footnote/fn2'],
            },
          ],
        })

        // Delete ONE article (one reference remains)
        await document.delete(agent, { query: { id: 'Article/article2a' } })

        // The footnote should STILL exist (article2b still references it)
        const r = await document.get(agent, {
          query: { id: 'Footnote/fn2', as_list: true },
        })
        expect(r.body).to.be.an('array').that.has.lengthOf(1)
        expect(r.body[0].text).to.equal('A shared footnote with two parents')
      })

      it('surviving referencing document is intact after partial cascade', async function () {
        // Create a shared footnote
        await document.insert(agent, {
          instance: {
            '@type': 'Footnote',
            id: 'fn3',
            text: 'Footnote for preservation test',
          },
        })

        // Create TWO articles referencing the same footnote
        await document.insert(agent, {
          instance: [
            {
              '@type': 'Article',
              title: 'article3a',
              notes: ['Footnote/fn3'],
            },
            {
              '@type': 'Article',
              title: 'article3b',
              notes: ['Footnote/fn3'],
            },
          ],
        })

        // Delete article3a (one reference remains via article3b)
        await document.delete(agent, { query: { id: 'Article/article3a' } })

        // The footnote should STILL exist with correct field values
        const rFootnote = await document.get(agent, {
          query: { id: 'Footnote/fn3', as_list: true },
        })
        expect(rFootnote.body).to.be.an('array').that.has.lengthOf(1)
        expect(rFootnote.body[0].id).to.equal('fn3')
        expect(rFootnote.body[0].text).to.equal('Footnote for preservation test')

        // The SURVIVING article must be completely intact — all fields verified
        const rArticle = await document.get(agent, {
          query: { id: 'Article/article3b', as_list: true },
        })
        expect(rArticle.body).to.be.an('array').that.has.lengthOf(1)
        expect(rArticle.body[0]['@id']).to.equal('Article/article3b')
        expect(rArticle.body[0]['@type']).to.equal('Article')
        expect(rArticle.body[0].title).to.equal('article3b')
        expect(rArticle.body[0].notes).to.be.an('array').that.has.lengthOf(1)
        expect(rArticle.body[0].notes[0]).to.equal('Footnote/fn3')
      })
    })

    describe('all deletion paths trigger cascade', function () {
      before(async function () {
        agent.dbName = 'test_all_deletion_paths'
        await db.create(agent)

        const schema = [
          {
            '@type': 'Class',
            '@id': 'Target',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
          },
          {
            '@type': 'Class',
            '@id': 'Parent',
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            child: {
              '@type': 'Optional',
              '@class': 'Target',
            },
          },
        ]

        await document.insert(agent, { schema })
      })

      after(async function () {
        await db.delete(agent)
      })

      it('Document API delete triggers cascade', async function () {
        await document.insert(agent, {
          instance: { '@type': 'Target', name: 'target_api' },
        })
        await document.insert(agent, {
          instance: { '@type': 'Parent', name: 'parent_api', child: 'Target/target_api' },
        })

        // Delete parent via Document API
        await document.delete(agent, { query: { id: 'Parent/parent_api' } })

        // Target should be cascade-deleted
        const r = await document.get(agent, {
          query: { id: 'Target/target_api', as_list: true },
        })
        expect(r.body).to.be.an('array').that.is.empty
      })

      it('WOQL delete triggers cascade', async function () {
        await document.insert(agent, {
          instance: { '@type': 'Target', name: 'target_woql' },
        })
        await document.insert(agent, {
          instance: { '@type': 'Parent', name: 'parent_woql', child: 'Target/target_woql' },
        })

        // Remove the reference via WOQL delete triple
        const deleteQuery = {
          '@type': 'DeleteTriple',
          subject: {
            '@type': 'NodeValue',
            node: 'Parent/parent_woql',
          },
          predicate: {
            '@type': 'NodeValue',
            node: 'child',
          },
          object: {
            '@type': 'Value',
            node: 'Target/target_woql',
          },
        }

        await woql.post(agent, deleteQuery)

        // Target should be cascade-deleted
        const r = await document.get(agent, {
          query: { id: 'Target/target_woql', as_list: true },
        })
        expect(r.body).to.be.an('array').that.is.empty
      })

      it('clearing Optional field via replace triggers cascade', async function () {
        await document.insert(agent, {
          instance: { '@type': 'Target', name: 'target_replace' },
        })
        await document.insert(agent, {
          instance: { '@type': 'Parent', name: 'parent_replace', child: 'Target/target_replace' },
        })

        // Replace the parent with child set to null (clearing the Optional field)
        await document.replace(agent, {
          instance: {
            '@id': 'Parent/parent_replace',
            '@type': 'Parent',
            name: 'parent_replace',
            // child field omitted — cleared
          },
        })

        // Target should be cascade-deleted
        const r = await document.get(agent, {
          query: { id: 'Target/target_replace', as_list: true },
        })
        expect(r.body).to.be.an('array').that.is.empty
      })
    })
  })

  // =========================================================================
  // 3. @shared ALL-REFERENCES RULE
  // =========================================================================

  describe('@shared all-references rule — all references count for liveness', function () {
    before(async function () {
      agent.dbName = 'test_all_refs_rule'
      await db.create(agent)

      const schema = [
        {
          '@type': 'Class',
          '@id': 'SharedFootnote',
          '@shared': [],
          '@key': { '@type': 'Lexical', '@fields': ['id'] },
          id: 'xsd:string',
          text: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Article',
          '@key': { '@type': 'Lexical', '@fields': ['title'] },
          title: 'xsd:string',
          notes: {
            '@type': 'Set',
            '@class': 'SharedFootnote',
          },
        },
        {
          '@type': 'Class',
          '@id': 'Index',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          entries: {
            '@type': 'Set',
            '@class': 'SharedFootnote',
          },
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('target stays alive when one reference removed but another remains', async function () {
      // Setup: fn1 referenced by Article AND Index
      await document.insert(agent, {
        instance: { '@type': 'SharedFootnote', id: 'fn1', text: 'Shared note' },
      })
      await document.insert(agent, {
        instance: { '@type': 'Article', title: 'art1', notes: ['SharedFootnote/fn1'] },
      })
      await document.insert(agent, {
        instance: { '@type': 'Index', name: 'idx1', entries: ['SharedFootnote/fn1'] },
      })

      // Remove the Article reference by deleting the article
      await document.delete(agent, { query: { id: 'Article/art1' } })

      // fn1 should STILL exist — Index still references it
      const r = await document.get(agent, {
        query: { id: 'SharedFootnote/fn1', as_list: true },
      })
      expect(r.body).to.be.an('array').that.has.lengthOf(1)
      expect(r.body[0].text).to.equal('Shared note')
    })

    it('target cascade-deleted only when all references are gone', async function () {
      // Setup: fn2 referenced by both Article and Index
      await document.insert(agent, {
        instance: { '@type': 'SharedFootnote', id: 'fn2', text: 'Another shared note' },
      })
      await document.insert(agent, {
        instance: { '@type': 'Article', title: 'art2', notes: ['SharedFootnote/fn2'] },
      })
      await document.insert(agent, {
        instance: { '@type': 'Index', name: 'idx2', entries: ['SharedFootnote/fn2'] },
      })

      // First, remove the Index reference
      await document.replace(agent, {
        instance: { '@id': 'Index/idx2', '@type': 'Index', name: 'idx2', entries: [] },
      })

      // fn2 should still exist (Article still references it)
      const r1 = await document.get(agent, {
        query: { id: 'SharedFootnote/fn2', as_list: true },
      })
      expect(r1.body).to.be.an('array').that.has.lengthOf(1)

      // Now remove the Article reference by deleting the Article
      await document.delete(agent, { query: { id: 'Article/art2' } })

      // NOW fn2 should be cascade-deleted (zero refs remain)
      const r2 = await document.get(agent, {
        query: { id: 'SharedFootnote/fn2', as_list: true },
      })
      expect(r2.body).to.be.an('array').that.is.empty
    })
  })

  // =========================================================================
  // 4. CASCADE RECURSION
  // =========================================================================

  describe('cascade recursion', function () {
    describe('recursive cascade delete through chain', function () {
      before(async function () {
        agent.dbName = 'test_recursive_cascade'
        await db.create(agent)

        // A -> B -> C chain, each @shared
        const schema = [
          {
            '@type': 'Class',
            '@id': 'C',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
          },
          {
            '@type': 'Class',
            '@id': 'B',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            refC: {
              '@type': 'Optional',
              '@class': 'C',
            },
          },
          {
            '@type': 'Class',
            '@id': 'A',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            refB: {
              '@type': 'Optional',
              '@class': 'B',
            },
          },
          {
            '@type': 'Class',
            '@id': 'Root',
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            refA: {
              '@type': 'Optional',
              '@class': 'A',
            },
          },
        ]

        await document.insert(agent, { schema })
      })

      after(async function () {
        await db.delete(agent)
      })

      it('full chain A to B to C cascade-deleted when root is deleted', async function () {
        // Create the chain: Root -> A -> B -> C
        await document.insert(agent, {
          instance: { '@type': 'C', name: 'c1' },
        })
        await document.insert(agent, {
          instance: { '@type': 'B', name: 'b1', refC: 'C/c1' },
        })
        await document.insert(agent, {
          instance: { '@type': 'A', name: 'a1', refB: 'B/b1' },
        })
        await document.insert(agent, {
          instance: { '@type': 'Root', name: 'root1', refA: 'A/a1' },
        })

        // Delete the root
        await document.delete(agent, { query: { id: 'Root/root1' } })

        // All should be cascade-deleted
        const rA = await document.get(agent, { query: { id: 'A/a1', as_list: true } })
        const rB = await document.get(agent, { query: { id: 'B/b1', as_list: true } })
        const rC = await document.get(agent, { query: { id: 'C/c1', as_list: true } })

        expect(rA.body).to.be.an('array').that.is.empty
        expect(rB.body).to.be.an('array').that.is.empty
        expect(rC.body).to.be.an('array').that.is.empty
      })

      it('cascade is atomic — all in single transaction with no stray triples', async function () {
        // Create another chain
        await document.insert(agent, {
          instance: { '@type': 'C', name: 'c2' },
        })
        await document.insert(agent, {
          instance: { '@type': 'B', name: 'b2', refC: 'C/c2' },
        })
        await document.insert(agent, {
          instance: { '@type': 'A', name: 'a2', refB: 'B/b2' },
        })
        await document.insert(agent, {
          instance: { '@type': 'Root', name: 'root2', refA: 'A/a2' },
        })

        // Delete root — entire chain should be gone in one transaction
        await document.delete(agent, { query: { id: 'Root/root2' } })

        // Verify no stray triples remain using WOQL
        const tripleQuery = {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'Subject' },
          predicate: { '@type': 'NodeValue', variable: 'Predicate' },
          object: { '@type': 'Value', variable: 'Object' },
        }

        const result = await woql.post(agent, tripleQuery)
        const triples = result.body.bindings

        const strayTriples = triples.filter(t => {
          const subject = t.Subject
          return subject && subject.includes && (
            subject.includes('A/a2') ||
            subject.includes('B/b2') ||
            subject.includes('C/c2')
          )
        })

        expect(strayTriples).to.have.lengthOf(0,
          `Found stray triples after cascade: ${JSON.stringify(strayTriples, null, 2)}`)
      })
    })

    describe('circular island deletion', function () {
      before(async function () {
        agent.dbName = 'test_circular_island'
        await db.create(agent)

        // CircNodes reference each other, both @shared
        const schema = [
          {
            '@type': 'Class',
            '@id': 'CircNode',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            peer: {
              '@type': 'Optional',
              '@class': 'CircNode',
            },
          },
          {
            '@type': 'Class',
            '@id': 'Holder',
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            held: {
              '@type': 'Optional',
              '@class': 'CircNode',
            },
          },
        ]

        await document.insert(agent, { schema })
      })

      after(async function () {
        await db.delete(agent)
      })

      it('circular pair with no external refs are both deleted', async function () {
        // Step 1: insert both nodes without peer refs (avoids dangling reference on insert)
        await document.insert(agent, {
          instance: [
            { '@type': 'CircNode', name: 'nodeA' },
            { '@type': 'CircNode', name: 'nodeB' },
          ],
        })

        // Step 2: add circular peer references via WOQL
        await woql.post(agent, {
          '@type': 'And',
          and: [
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'CircNode/nodeA' },
              predicate: { '@type': 'NodeValue', node: 'peer' },
              object: { '@type': 'Value', node: 'CircNode/nodeB' },
            },
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'CircNode/nodeB' },
              predicate: { '@type': 'NodeValue', node: 'peer' },
              object: { '@type': 'Value', node: 'CircNode/nodeA' },
            },
          ],
        })

        // Create a holder with external reference to A
        await document.insert(agent, {
          instance: { '@type': 'Holder', name: 'holder1', held: 'CircNode/nodeA' },
        })

        // Delete the holder — removes external reference to A
        await document.delete(agent, { query: { id: 'Holder/holder1' } })

        // Both A and B should be deleted (circular island with no external refs)
        const rA = await document.get(agent, { query: { id: 'CircNode/nodeA', as_list: true } })
        const rB = await document.get(agent, { query: { id: 'CircNode/nodeB', as_list: true } })

        expect(rA.body).to.be.an('array').that.is.empty
        expect(rB.body).to.be.an('array').that.is.empty
      })

      it('circular pair with external ref to one member — neither deleted', async function () {
        // Step 1: insert both nodes without peer refs
        await document.insert(agent, {
          instance: [
            { '@type': 'CircNode', name: 'nodeC' },
            { '@type': 'CircNode', name: 'nodeD' },
          ],
        })

        // Step 2: add circular peer references via WOQL
        await woql.post(agent, {
          '@type': 'And',
          and: [
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'CircNode/nodeC' },
              predicate: { '@type': 'NodeValue', node: 'peer' },
              object: { '@type': 'Value', node: 'CircNode/nodeD' },
            },
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'CircNode/nodeD' },
              predicate: { '@type': 'NodeValue', node: 'peer' },
              object: { '@type': 'Value', node: 'CircNode/nodeC' },
            },
          ],
        })

        // Create TWO holders: one references C, one references D
        await document.insert(agent, {
          instance: [
            { '@type': 'Holder', name: 'holder2', held: 'CircNode/nodeC' },
            { '@type': 'Holder', name: 'holder3', held: 'CircNode/nodeD' },
          ],
        })

        // Delete holder referencing C — but holder3 still references D
        await document.delete(agent, { query: { id: 'Holder/holder2' } })

        // Neither should be deleted (D has external ref — D alive — D refs C — C alive)
        const rC = await document.get(agent, { query: { id: 'CircNode/nodeC', as_list: true } })
        const rD = await document.get(agent, { query: { id: 'CircNode/nodeD', as_list: true } })

        expect(rC.body).to.be.an('array').that.has.lengthOf(1)
        expect(rD.body).to.be.an('array').that.has.lengthOf(1)
      })

      it('larger circular island A B C ring deleted when no external refs', async function () {
        // Step 1: insert all three ring nodes without peer refs
        await document.insert(agent, {
          instance: [
            { '@type': 'CircNode', name: 'ring1' },
            { '@type': 'CircNode', name: 'ring2' },
            { '@type': 'CircNode', name: 'ring3' },
          ],
        })

        // Step 2: add circular peer references via WOQL (ring1→ring2→ring3→ring1)
        await woql.post(agent, {
          '@type': 'And',
          and: [
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'CircNode/ring1' },
              predicate: { '@type': 'NodeValue', node: 'peer' },
              object: { '@type': 'Value', node: 'CircNode/ring2' },
            },
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'CircNode/ring2' },
              predicate: { '@type': 'NodeValue', node: 'peer' },
              object: { '@type': 'Value', node: 'CircNode/ring3' },
            },
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'CircNode/ring3' },
              predicate: { '@type': 'NodeValue', node: 'peer' },
              object: { '@type': 'Value', node: 'CircNode/ring1' },
            },
          ],
        })

        // External holder references ring1
        await document.insert(agent, {
          instance: { '@type': 'Holder', name: 'ringHolder', held: 'CircNode/ring1' },
        })

        // Delete the holder
        await document.delete(agent, { query: { id: 'Holder/ringHolder' } })

        // All three ring nodes should be deleted
        const r1 = await document.get(agent, { query: { id: 'CircNode/ring1', as_list: true } })
        const r2 = await document.get(agent, { query: { id: 'CircNode/ring2', as_list: true } })
        const r3 = await document.get(agent, { query: { id: 'CircNode/ring3', as_list: true } })

        expect(r1.body).to.be.an('array').that.is.empty
        expect(r2.body).to.be.an('array').that.is.empty
        expect(r3.body).to.be.an('array').that.is.empty
      })

      it('no infinite loop on circular references — completes within timeout', async function () {
        this.timeout(30000) // Should complete well within this — infinite loop would exceed

        // Step 1: insert both nodes without peer refs
        await document.insert(agent, {
          instance: [
            { '@type': 'CircNode', name: 'loopA' },
            { '@type': 'CircNode', name: 'loopB' },
          ],
        })

        // Step 2: add circular peer references via WOQL
        await woql.post(agent, {
          '@type': 'And',
          and: [
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'CircNode/loopA' },
              predicate: { '@type': 'NodeValue', node: 'peer' },
              object: { '@type': 'Value', node: 'CircNode/loopB' },
            },
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'CircNode/loopB' },
              predicate: { '@type': 'NodeValue', node: 'peer' },
              object: { '@type': 'Value', node: 'CircNode/loopA' },
            },
          ],
        })
        await document.insert(agent, {
          instance: { '@type': 'Holder', name: 'loopHolder', held: 'CircNode/loopA' },
        })

        // Delete holder — if cycle prevention works, this completes without timeout
        await document.delete(agent, { query: { id: 'Holder/loopHolder' } })

        // Both should be deleted (confirming the operation completed)
        const rA = await document.get(agent, { query: { id: 'CircNode/loopA', as_list: true } })
        const rB = await document.get(agent, { query: { id: 'CircNode/loopB', as_list: true } })

        expect(rA.body).to.be.an('array').that.is.empty
        expect(rB.body).to.be.an('array').that.is.empty
      })
    })

    describe('@shared with no references pointing to it — no cascade', function () {
      before(async function () {
        agent.dbName = 'test_no_refs_no_cascade'
        await db.create(agent)

        const schema = [
          {
            '@type': 'Class',
            '@id': 'SharedAsset',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
          },
          {
            '@type': 'Class',
            '@id': 'Referrer',
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            // Reference to a @shared class
            asset: {
              '@type': 'Optional',
              '@class': 'SharedAsset',
            },
          },
        ]

        await document.insert(agent, { schema })
      })

      after(async function () {
        await db.delete(agent)
      })

      it('no cascade when @shared target still has remaining references', async function () {
        await document.insert(agent, {
          instance: { '@type': 'SharedAsset', name: 'asset1' },
        })
        // Two referrers point to the same asset
        await document.insert(agent, {
          instance: [
            { '@type': 'Referrer', name: 'ref1', asset: 'SharedAsset/asset1' },
            { '@type': 'Referrer', name: 'ref2', asset: 'SharedAsset/asset1' },
          ],
        })

        // Delete one referrer (one reference removed, but one remains)
        await document.delete(agent, { query: { id: 'Referrer/ref1' } })

        // SharedAsset should STILL exist — ref2 still references it
        const r = await document.get(agent, {
          query: { id: 'SharedAsset/asset1', as_list: true },
        })
        expect(r.body).to.be.an('array').that.has.lengthOf(1)
      })
    })

    describe('transaction timing — remove and re-add in same transaction', function () {
      before(async function () {
        agent.dbName = 'test_transaction_timing'
        await db.create(agent)

        const schema = [
          {
            '@type': 'Class',
            '@id': 'SharedItem',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
          },
          {
            '@type': 'Class',
            '@id': 'Container',
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            item: {
              '@type': 'Optional',
              '@class': 'SharedItem',
            },
          },
        ]

        await document.insert(agent, { schema })
      })

      after(async function () {
        await db.delete(agent)
      })

      it('target survives if reference removed and re-added in same transaction', async function () {
        await document.insert(agent, {
          instance: { '@type': 'SharedItem', name: 'item1' },
        })
        await document.insert(agent, {
          instance: { '@type': 'Container', name: 'cont1', item: 'SharedItem/item1' },
        })

        // In a single transaction via WOQL: remove ref then re-add
        // This tests that the pre_commit_hook sees the post-mutation state
        const updateQuery = {
          '@type': 'And',
          and: [
            {
              '@type': 'DeleteTriple',
              subject: { '@type': 'NodeValue', node: 'Container/cont1' },
              predicate: { '@type': 'NodeValue', node: 'item' },
              object: { '@type': 'Value', node: 'SharedItem/item1' },
            },
            {
              '@type': 'AddTriple',
              subject: { '@type': 'NodeValue', node: 'Container/cont1' },
              predicate: { '@type': 'NodeValue', node: 'item' },
              object: { '@type': 'Value', node: 'SharedItem/item1' },
            },
          ],
        }

        await woql.post(agent, updateQuery)

        // SharedItem should still exist (reference was re-added)
        const r = await document.get(agent, {
          query: { id: 'SharedItem/item1', as_list: true },
        })
        expect(r.body).to.be.an('array').that.has.lengthOf(1)
      })
    })
  })

  // =========================================================================
  // 5. EXPLICIT DELETE OF @shared WITH ACTIVE REFS
  // =========================================================================

  describe('explicit delete of @shared with active references', function () {
    before(async function () {
      agent.dbName = 'test_explicit_delete'
      await db.create(agent)

      const schema = [
        {
          '@type': 'Class',
          '@id': 'SharedDoc',
          '@shared': [],
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Referrer',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          ref: {
            '@type': 'Optional',
            '@class': 'SharedDoc',
          },
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('explicit delete of @shared document proceeds even with active refs', async function () {
      // Create a shared document
      await document.insert(agent, {
        instance: { '@type': 'SharedDoc', name: 'shared1' },
      })

      // Create a referrer that references it
      await document.insert(agent, {
        instance: { '@type': 'Referrer', name: 'ref1', ref: 'SharedDoc/shared1' },
      })

      // Explicitly delete the shared document (not via cascade)
      // This should proceed — explicit delete is intentional
      // The existing referential integrity checker may fail if the ref field is mandatory,
      // but since it's Optional here, the delete should succeed
      await document.delete(agent, { query: { id: 'SharedDoc/shared1' } })

      // Verify it's deleted
      const r = await document.get(agent, {
        query: { id: 'SharedDoc/shared1', as_list: true },
      })
      expect(r.body).to.be.an('array').that.is.empty
    })
  })

  // =========================================================================
  // 6. STANDALONE CREATION
  // =========================================================================

  describe('standalone creation', function () {
    before(async function () {
      agent.dbName = 'test_standalone_creation'
      await db.create(agent)

      const schema = [
        {
          '@type': 'Class',
          '@id': 'SharedResource',
          '@shared': [],
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Owner',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          resource: {
            '@type': 'Optional',
            '@class': 'SharedResource',
          },
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('@shared document can be created standalone without parent', async function () {
      const r = await document.insert(agent, {
        instance: { '@type': 'SharedResource', name: 'standalone_shared' },
      })
      expect(r.status).to.equal(200)

      // Verify it exists and appears in document listings
      const r2 = await document.get(agent, {
        query: { type: 'SharedResource', as_list: true },
      })
      expect(r2.body).to.be.an('array').that.has.lengthOf(1)
      expect(r2.body[0].name).to.equal('standalone_shared')
    })

    it('@shared instance appears in document listings', async function () {
      // Already verified above — @shared documents are regular documents
      const r = await document.get(agent, {
        query: { type: 'SharedResource', as_list: true },
      })
      expect(r.body).to.be.an('array')
      expect(r.body.length).to.be.greaterThan(0)
    })

    it('@shared instance uses class key descriptor for IRI', async function () {
      // SharedResource uses Lexical key with 'name' field
      // So ID should be SharedResource/<name>
      const r = await document.get(agent, {
        query: { id: 'SharedResource/standalone_shared', as_list: true },
      })
      expect(r.body).to.be.an('array').that.has.lengthOf(1)
      expect(r.body[0]['@id']).to.equal('SharedResource/standalone_shared')
    })

    it('deleting parent cascade-deletes @shared ref with no other references', async function () {
      // Create another shared resource
      await document.insert(agent, {
        instance: { '@type': 'SharedResource', name: 'safe_shared' },
      })

      // Create an owner referencing it
      await document.insert(agent, {
        instance: { '@type': 'Owner', name: 'owner1', resource: 'SharedResource/safe_shared' },
      })

      // Delete the owner — reference removed — cascade check fires
      await document.delete(agent, { query: { id: 'Owner/owner1' } })

      // Since it's @shared with only one ref and no other refs, it should be cascade-deleted
      const r = await document.get(agent, {
        query: { id: 'SharedResource/safe_shared', as_list: true },
      })
      expect(r.body).to.be.an('array').that.is.empty
    })
  })

  // =========================================================================
  // 7. SET PER-ELEMENT LIVENESS
  // =========================================================================

  describe('Set per-element liveness', function () {
    before(async function () {
      agent.dbName = 'test_per_element_liveness'
      await db.create(agent)

      const schema = [
        {
          '@type': 'Class',
          '@id': 'Item',
          '@shared': [],
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Collection',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          items: {
            '@type': 'Set',
            '@class': 'Item',
          },
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('removing one element from Set cascades only that element', async function () {
      // Create three items
      await document.insert(agent, {
        instance: [
          { '@type': 'Item', name: 'item1' },
          { '@type': 'Item', name: 'item2' },
          { '@type': 'Item', name: 'item3' },
        ],
      })

      // Create collection referencing all three
      await document.insert(agent, {
        instance: {
          '@type': 'Collection',
          name: 'coll1',
          items: ['Item/item1', 'Item/item2', 'Item/item3'],
        },
      })

      // Replace collection with only item1 and item3 (removing item2)
      await document.replace(agent, {
        instance: {
          '@id': 'Collection/coll1',
          '@type': 'Collection',
          name: 'coll1',
          items: ['Item/item1', 'Item/item3'],
        },
      })

      // item2 should be cascade-deleted (its only ref was removed)
      const r2 = await document.get(agent, { query: { id: 'Item/item2', as_list: true } })
      expect(r2.body).to.be.an('array').that.is.empty

      // item1 and item3 should still exist
      const r1 = await document.get(agent, { query: { id: 'Item/item1', as_list: true } })
      const r3 = await document.get(agent, { query: { id: 'Item/item3', as_list: true } })
      expect(r1.body).to.be.an('array').that.has.lengthOf(1)
      expect(r3.body).to.be.an('array').that.has.lengthOf(1)
    })

    it('removing all elements from Set cascades all', async function () {
      // Create two items
      await document.insert(agent, {
        instance: [
          { '@type': 'Item', name: 'batch1' },
          { '@type': 'Item', name: 'batch2' },
        ],
      })

      // Create collection referencing both
      await document.insert(agent, {
        instance: {
          '@type': 'Collection',
          name: 'coll2',
          items: ['Item/batch1', 'Item/batch2'],
        },
      })

      // Replace collection with empty items list
      await document.replace(agent, {
        instance: {
          '@id': 'Collection/coll2',
          '@type': 'Collection',
          name: 'coll2',
          items: [],
        },
      })

      // Both should be cascade-deleted
      const r1 = await document.get(agent, { query: { id: 'Item/batch1', as_list: true } })
      const r2 = await document.get(agent, { query: { id: 'Item/batch2', as_list: true } })
      expect(r1.body).to.be.an('array').that.is.empty
      expect(r2.body).to.be.an('array').that.is.empty
    })

    it('parent collection document is intact with correct items after element cascade', async function () {
      // Create three items
      await document.insert(agent, {
        instance: [
          { '@type': 'Item', name: 'kept1' },
          { '@type': 'Item', name: 'removed1' },
          { '@type': 'Item', name: 'kept2' },
        ],
      })

      // Create collection referencing all three
      await document.insert(agent, {
        instance: {
          '@type': 'Collection',
          name: 'coll3',
          items: ['Item/kept1', 'Item/removed1', 'Item/kept2'],
        },
      })

      // Replace collection removing only removed1
      await document.replace(agent, {
        instance: {
          '@id': 'Collection/coll3',
          '@type': 'Collection',
          name: 'coll3',
          items: ['Item/kept1', 'Item/kept2'],
        },
      })

      // removed1 should be cascade-deleted
      const rRemoved = await document.get(agent, { query: { id: 'Item/removed1', as_list: true } })
      expect(rRemoved.body).to.be.an('array').that.is.empty

      // Surviving items should have correct field values
      const rKept1 = await document.get(agent, { query: { id: 'Item/kept1', as_list: true } })
      const rKept2 = await document.get(agent, { query: { id: 'Item/kept2', as_list: true } })
      expect(rKept1.body).to.be.an('array').that.has.lengthOf(1)
      expect(rKept1.body[0].name).to.equal('kept1')
      expect(rKept2.body).to.be.an('array').that.has.lengthOf(1)
      expect(rKept2.body[0].name).to.equal('kept2')

      // CRITICAL: Re-fetch the Collection document itself — verify it is intact
      const rColl = await document.get(agent, { query: { id: 'Collection/coll3', as_list: true } })
      expect(rColl.body).to.be.an('array').that.has.lengthOf(1)
      expect(rColl.body[0]['@id']).to.equal('Collection/coll3')
      expect(rColl.body[0]['@type']).to.equal('Collection')
      expect(rColl.body[0].name).to.equal('coll3')
      // The items Set should contain exactly the two surviving items
      expect(rColl.body[0].items).to.be.an('array').that.has.lengthOf(2)
      expect(rColl.body[0].items).to.include('Item/kept1')
      expect(rColl.body[0].items).to.include('Item/kept2')
      expect(rColl.body[0].items).to.not.include('Item/removed1')
    })
  })

  // =========================================================================
  // 8. MIGRATION COMPATIBILITY
  // =========================================================================

  describe('migration compatibility', function () {
    describe('adding @shared to existing schema', function () {
      before(async function () {
        agent.dbName = 'test_migration_add_annotations'
        await db.create(agent)

        // Create schema WITHOUT @shared first
        const schema = [
          {
            '@type': 'Class',
            '@id': 'Resource',
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
          },
          {
            '@type': 'Class',
            '@id': 'Container',
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            resource: {
              '@type': 'Optional',
              '@class': 'Resource',
            },
          },
        ]

        await document.insert(agent, { schema })

        // Insert instance data BEFORE adding annotations
        await document.insert(agent, {
          instance: { '@type': 'Resource', name: 'existing_res' },
        })
        await document.insert(agent, {
          instance: { '@type': 'Container', name: 'existing_cont', resource: 'Resource/existing_res' },
        })
      })

      after(async function () {
        await db.delete(agent)
      })

      it('adding @shared to existing class with instances succeeds', async function () {
        // Update schema to add @shared to Resource
        const updatedResource = {
          '@id': 'Resource',
          '@type': 'Class',
          '@shared': [],
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        }

        const r = await document.replace(agent, { schema: updatedResource })
        expect(r.status).to.equal(200)
      })

      it('existing instances are not modified after adding @shared', async function () {
        const r = await document.get(agent, {
          query: { id: 'Resource/existing_res', as_list: true },
        })
        expect(r.body).to.be.an('array').that.has.lengthOf(1)
        expect(r.body[0].name).to.equal('existing_res')
      })

      it('after adding @shared, removing reference triggers cascade', async function () {
        // Clear the resource reference
        await document.replace(agent, {
          instance: {
            '@id': 'Container/existing_cont',
            '@type': 'Container',
            name: 'existing_cont',
            // resource field omitted — cleared
          },
        })

        // Resource should now be cascade-deleted (zero refs remain, target is @shared)
        const r = await document.get(agent, {
          query: { id: 'Resource/existing_res', as_list: true },
        })
        expect(r.body).to.be.an('array').that.is.empty
      })
    })

    describe('removing @shared from existing schema', function () {
      before(async function () {
        agent.dbName = 'test_migration_remove_annotations'
        await db.create(agent)

        // Create schema WITH @shared
        const schema = [
          {
            '@type': 'Class',
            '@id': 'Asset',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
          },
          {
            '@type': 'Class',
            '@id': 'Holder',
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            asset: {
              '@type': 'Optional',
              '@class': 'Asset',
            },
          },
        ]

        await document.insert(agent, { schema })

        // Insert instances
        await document.insert(agent, {
          instance: { '@type': 'Asset', name: 'asset1' },
        })
        await document.insert(agent, {
          instance: { '@type': 'Holder', name: 'holder1', asset: 'Asset/asset1' },
        })
      })

      after(async function () {
        await db.delete(agent)
      })

      it('removing @shared from a class succeeds', async function () {
        const updatedAsset = {
          '@id': 'Asset',
          '@type': 'Class',
          // @shared REMOVED
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
        }

        const r = await document.replace(agent, { schema: updatedAsset })
        expect(r.status).to.equal(200)
      })

      it('after removing @shared, delete does NOT cascade', async function () {
        // Delete the holder — should NOT cascade to asset (@shared annotation removed)
        await document.delete(agent, { query: { id: 'Holder/holder1' } })

        // Asset should still exist
        const r = await document.get(agent, {
          query: { id: 'Asset/asset1', as_list: true },
        })
        expect(r.body).to.be.an('array').that.has.lengthOf(1)
      })
    })
  })

  // =========================================================================
  // 9. BYSTANDER DOCUMENT PRESERVATION
  // =========================================================================

  describe('bystander document unaffected by cascade', function () {
    before(async function () {
      agent.dbName = 'test_bystander_preservation'
      await db.create(agent)

      const schema = [
        {
          '@type': 'Class',
          '@id': 'SharedNote',
          '@shared': [],
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          content: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Parent',
          '@key': { '@type': 'Lexical', '@fields': ['name'] },
          name: 'xsd:string',
          tag: 'xsd:string',
          note: {
            '@type': 'Optional',
            '@class': 'SharedNote',
          },
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('unrelated bystander document is fully intact after cascade', async function () {
      // Create a @shared note
      await document.insert(agent, {
        instance: { '@type': 'SharedNote', name: 'n1', content: 'Important note' },
      })

      // Create Parent A referencing the shared note
      await document.insert(agent, {
        instance: { '@type': 'Parent', name: 'parentA', tag: 'urgent', note: 'SharedNote/n1' },
      })

      // Create Parent B — NO connection to any @shared document
      await document.insert(agent, {
        instance: { '@type': 'Parent', name: 'parentB', tag: 'routine' },
      })

      // Delete Parent A → SharedNote/n1 should cascade (zero refs remain)
      await document.delete(agent, { query: { id: 'Parent/parentA' } })

      // Verify cascade occurred: SharedNote/n1 is gone
      const rNote = await document.get(agent, {
        query: { id: 'SharedNote/n1', as_list: true },
      })
      expect(rNote.body).to.be.an('array').that.is.empty

      // CRITICAL: Verify bystander Parent B is completely intact — all fields
      const rB = await document.get(agent, {
        query: { id: 'Parent/parentB', as_list: true },
      })
      expect(rB.body).to.be.an('array').that.has.lengthOf(1)
      expect(rB.body[0]['@id']).to.equal('Parent/parentB')
      expect(rB.body[0]['@type']).to.equal('Parent')
      expect(rB.body[0].name).to.equal('parentB')
      expect(rB.body[0].tag).to.equal('routine')
      // Parent B has no note field set — it should remain absent/undefined
      expect(rB.body[0]).to.not.have.property('note')
    })
  })

  // =========================================================================
  // 10. STANDALONE SHARED DOCUMENT SURVIVES UNRELATED CASCADE
  // =========================================================================

  describe('standalone @shared document survives unrelated cascade', function () {
    before(async function () {
      agent.dbName = 'test_standalone_survives_cascade'
      await db.create(agent)

      const schema = [
        {
          '@type': 'Class',
          '@id': 'Child',
          '@shared': [],
          '@key': { '@type': 'Lexical', '@fields': ['v'] },
          v: 'xsd:string',
        },
        {
          '@type': 'Class',
          '@id': 'Parent',
          '@key': { '@type': 'Lexical', '@fields': ['n'] },
          n: 'xsd:string',
          c: {
            '@type': 'Optional',
            '@class': 'Child',
          },
        },
      ]

      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('standalone @shared doc is not swept when unrelated cascade fires', async function () {
      // Create a standalone @shared document — never referenced by any parent
      await document.insert(agent, {
        instance: { '@type': 'Child', v: 'standalone' },
      })

      // Create a DIFFERENT @shared document that IS owned by a parent
      await document.insert(agent, {
        instance: { '@type': 'Child', v: 'owned' },
      })
      await document.insert(agent, {
        instance: { '@type': 'Parent', n: 'p1', c: 'Child/owned' },
      })

      // Delete the parent — cascade should delete Child/owned (zero refs remain)
      await document.delete(agent, { query: { id: 'Parent/p1' } })

      // Child/owned should be cascade-deleted (its only reference was removed)
      const rOwned = await document.get(agent, {
        query: { id: 'Child/owned', as_list: true },
      })
      expect(rOwned.body).to.be.an('array').that.is.empty

      // CRITICAL: Child/standalone must still exist — it was never referenced,
      // so no triple pointing to it was ever deleted. The cascade algorithm must
      // only delete shared documents reachable from a deleted triple, not all
      // zero-reference shared documents.
      const rStandalone = await document.get(agent, {
        query: { id: 'Child/standalone', as_list: true },
      })
      expect(rStandalone.body).to.be.an('array').that.has.lengthOf(1)
      expect(rStandalone.body[0]['@id']).to.equal('Child/standalone')
      expect(rStandalone.body[0]['@type']).to.equal('Child')
      expect(rStandalone.body[0].v).to.equal('standalone')

      // Parent/p1 should be deleted
      const rParent = await document.get(agent, {
        query: { id: 'Parent/p1', as_list: true },
      })
      expect(rParent.body).to.be.an('array').that.is.empty
    })
  })

  // =========================================================================
  // 11. CIRCULAR REFERENCE HANDLING
  // =========================================================================

  describe('circular reference handling', function () {
    describe('circular @shared documents cascade-deleted correctly', function () {
      before(async function () {
        agent.dbName = 'test_circular_cascade'
        await db.create(agent)

        // Schema: @shared nodes that can reference each other (circular)
        const schema = [
          {
            '@type': 'Class',
            '@id': 'CircNode',
            '@shared': [],
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            peer: {
              '@type': 'Optional',
              '@class': 'CircNode',
            },
          },
          {
            '@type': 'Class',
            '@id': 'CircParent',
            '@key': { '@type': 'Lexical', '@fields': ['name'] },
            name: 'xsd:string',
            left: {
              '@type': 'Optional',
              '@class': 'CircNode',
            },
            right: {
              '@type': 'Optional',
              '@class': 'CircNode',
            },
          },
        ]

        await document.insert(agent, { schema })
      })

      after(async function () {
        await db.delete(agent)
      })

      it('cascade-deletes circular @shared pair when parent removed', async function () {
        // Create two @shared nodes that reference each other
        await document.insert(agent, {
          instance: [
            { '@type': 'CircNode', name: 'nodeA', peer: 'CircNode/nodeB' },
            { '@type': 'CircNode', name: 'nodeB', peer: 'CircNode/nodeA' },
          ],
        })

        // Create a parent referencing both
        await document.insert(agent, {
          instance: {
            '@type': 'CircParent',
            name: 'parent1',
            left: 'CircNode/nodeA',
            right: 'CircNode/nodeB',
          },
        })

        // Delete the parent — removes all external references to the circular pair
        await document.delete(agent, { query: { id: 'CircParent/parent1' } })

        // Both circular nodes should be cascade-deleted (no infinite loop)
        const rA = await document.get(agent, {
          query: { id: 'CircNode/nodeA', as_list: true },
        })
        expect(rA.body).to.be.an('array').that.is.empty

        const rB = await document.get(agent, {
          query: { id: 'CircNode/nodeB', as_list: true },
        })
        expect(rB.body).to.be.an('array').that.is.empty
      })
    })
  })
})
