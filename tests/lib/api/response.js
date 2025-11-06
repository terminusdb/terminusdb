const { expect } = require('chai')

const error = require('./error.js')
const { Params } = require('../params.js')
const util = require('../util.js')

function verify (params) {
  params = new Params(params)
  const expected = {
    status: params.integerRequired('status'),
    fun: params.fun('fun'),
    body: params.object('body'),
  }
  params.assertEmpty()

  return function (r) {
    expect(r).to.have.property('status', expected.status)

    if (util.isDefined(expected.body)) {
      const entries1 = Object.entries(expected.body)
      if (entries1.length > 0) {
        expect(r).to.have.property('body')
      }
      for (const [key1, value1] of entries1) {
        expect(r.body).to.have.property(key1)
        if (util.isObject(value1)) {
          for (const [key2, value2] of Object.entries(value1)) {
            expect(r.body[key1]).to.have.deep.property(key2, value2)
          }
        } else {
          expect(r.body[key1]).to.equal(value1)
        }
      }
    }

    if (util.isDefined(expected.fun)) {
      expected.fun(r)
    }

    return r
  }
}

function verifyDocInsertSuccess (r) {
  expect(r).to.have.property('body').that.is.an('array')
  expect(r).to.have.property('request')
  expect(r.request).to.have.property('_data')

  // Verify the `@id` values are the ones expected.
  if (Array.isArray(r.request._data)) {
    let data = r.request._data

    // Support fullReplace with @context as the first element.
    if (data.length > 0 && data[0]['@type'] === '@context') {
      data = data.slice(1)
    }

    // Debug logging for mismatches
    if (r.body.length !== data.length) {
      console.error('❌ UNVERIFIED: Response body length mismatch')
      console.error('   Expected:', data.length, 'documents')
      console.error('   Received:', r.body.length, 'documents')
      console.error('   Request data:', JSON.stringify(data, null, 2))
      console.error('   Response body:', JSON.stringify(r.body, null, 2))
      console.error('   Response status:', r.status)
      console.error('   Response headers:', r.headers)
    }
    expect(r.body.length).to.equal(data.length)

    for (let i = 0; i < r.body.length; i++) {
      verifyDocId(data[i]['@id'], r.body[i])
    }
  } else if (util.isObject(r.request._data)) {
    expect(r.body.length).to.equal(1)
    verifyDocId(r.request._data['@id'], r.body[0])
  }
  return r
}

// Verify that, if a request includes an `@id`, that value is the suffix of the
// value in the response.
function verifyDocId (requestId, responseId) {
  if (requestId) {
    // The request ID may contain regular expression symbols, so we escape them
    // according to:
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#escaping
    requestId = requestId.replace(/[.*+?^${}()|[\]\\]/g, '\\$&') + '$'
    expect(responseId).to.match(new RegExp(requestId))
  }
}

module.exports = {
  verify,
  incorrectAuthentication: {
    status: 401,
    body: {
      '@type': 'api:ErrorResponse',
      'api:status': 'api:failure',
      'api:error': {
        '@type': 'api:IncorrectAuthenticationError',
      },
    },
  },
  invalidJSON (objectString) {
    return {
      status: 400,
      body: {
        'api:status': 'api:failure',
        'api:message': 'Submitted object was not valid JSON',
        'system:object': objectString,
      },
    }
  },
  duplicateField: {
    status: 400,
    body: {
      '@type': 'api:DuplicateField',
      'api:status': 'api:failure',
    },
  },
  db: {
    createSuccess: {
      status: 200,
      body: {
        '@type': 'api:DbCreateResponse',
        'api:status': 'api:success',
      },
    },
    createFailure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:DbCreateErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    createNotFound (errorObject) {
      const result = {
        status: 404,
        body: {
          '@type': 'api:DbCreateErrorResponse',
          'api:status': 'api:not_found',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    existsSuccess: {
      status: 200,
    },
    existsFailure: {
      status: 400,
    },
    existsNotFound: {
      status: 404,
    },
    deleteSuccess: {
      status: 200,
      body: {
        '@type': 'api:DbDeleteResponse',
        'api:status': 'api:success',
      },
    },
    deleteFailure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:DbDeleteErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    deleteNotFound (errorObject) {
      const result = {
        status: 404,
        body: {
          '@type': 'api:DbDeleteErrorResponse',
          'api:status': 'api:not_found',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
  },
  doc: {
    getSuccess: {
      status: 200,
    },
    getFailure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:GetDocumentErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    insertSuccess: {
      status: 200,
      fun: verifyDocInsertSuccess,
    },
    insertFailure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:InsertDocumentErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    get replaceSuccess () {
      return this.insertSuccess
    },
    replaceFailure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:ReplaceDocumentErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    replaceNotFound (document) {
      return {
        status: 404,
        body: {
          '@type': 'api:ReplaceDocumentErrorResponse',
          'api:status': 'api:not_found',
          'api:error': error.documentNotFound(document['@id'], document),
        },
      }
    },
    deleteSuccess: {
      status: 204,
    },
    deleteFailure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:DeleteDocumentErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    deleteNotFound (id) {
      return {
        status: 404,
        body: {
          '@type': 'api:DeleteDocumentErrorResponse',
          'api:status': 'api:not_found',
          'api:error': error.documentNotFound(id),
        },
      }
    },
  },
  org: {
    addSuccess: {
      status: 200,
      body: {
        '@type': 'api:AddOrganizationResponse',
        'api:status': 'api:success',
      },
    },
    addFailure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:AddOrganizationErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    deleteSuccess: {
      status: 200,
      body: {
        '@type': 'api:DeleteOrganizationResponse',
        'api:status': 'api:success',
      },
    },
    deleteFailure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:DeleteOrganizationErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    deleteNotFound (errorObject) {
      const result = {
        status: 404,
        body: {
          '@type': 'api:DeleteOrganizationErrorResponse',
          'api:status': 'api:not_found',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
  },
  branch: {
    success: {
      status: 200,
      body: {
        '@type': 'api:BranchResponse',
        'api:status': 'api:success',
      },
    },
    failure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:BranchErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
  },
  triples: {
    getSuccess: {
      status: 200,
    },
    insertSuccess: {
      status: 200,
      body: {
        '@type': 'api:TriplesInsertResponse',
        'api:status': 'api:success',
      },
    },
    updateSuccess: {
      status: 200,
      body: {
        '@type': 'api:TriplesUpdateResponse',
        'api:status': 'api:success',
      },
    },
    failure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:TriplesErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
  },
  remote: {
    success: {
      status: 200,
      body: {
        '@type': 'api:RemoteResponse',
        'api:status': 'api:success',
      },
    },
    failure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:RemoteErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    notFound (errorObject) {
      const result = {
        status: 404,
        body: {
          '@type': 'api:RemoteErrorResponse',
          'api:status': 'api:not_found',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
  },
  woql: {
    success: {
      status: 200,
      body: {
        '@type': 'api:WoqlResponse',
        'api:status': 'api:success',
      },
    },
    failure (errorObject) {
      const result = {
        status: 400,
        body: {
          '@type': 'api:WoqlErrorResponse',
          'api:status': 'api:failure',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
    notFound (errorObject) {
      const result = {
        status: 404,
        body: {
          '@type': 'api:WoqlErrorResponse',
          'api:status': 'api:not_found',
        },
      }
      if (util.isDefined(errorObject)) {
        result.body['api:error'] = errorObject
      }
      return result
    },
  },
}
