const util = require('../util.js')

module.exports = {
  missingParameter (parameter) {
    return {
      '@type': 'api:MissingParameter',
      'api:parameter': parameter,
    }
  },
  badParameterType (name, type, value) {
    return {
      '@type': 'api:BadParameterType',
      'api:parameter': name,
      'api:expected_type': type,
      'api:value': value,
    }
  },
  missingField (field, document) {
    return {
      '@type': 'api:MissingField',
      'api:field': field,
      'api:document': document,
    }
  },
  badFieldValue (field, value, document) {
    return {
      '@type': 'api:BadFieldValue',
      'api:field': field,
      'api:value': value,
      'api:document': document,
    }
  },
  unknownUser (user) {
    return {
      '@type': 'api:UnknownUser',
      'api:user_name': user,
    }
  },
  unknownDatabase (orgName, dbName) {
    return {
      '@type': 'api:UnknownDatabase',
      'api:organization_name': orgName,
      'api:database_name': dbName,
    }
  },
  invalidDatabaseName (name) {
    return {
      '@type': 'api:InvalidDatabaseName',
      'api:database_name': name,
    }
  },
  unknownOrganization (name) {
    return {
      '@type': 'api:UnknownOrganizationName',
      'api:organization_name': name,
    }
  },
  invalidOrganizationName (name) {
    return {
      '@type': 'api:InvalidOrganizationName',
      'api:organization_name': name,
    }
  },
  invalidPrefix (name, value) {
    return {
      '@type': 'api:InvalidPrefix',
      'api:prefix_name': name,
      'api:prefix_value': value,
    }
  },
  schemaCheckFailure (witnesses) {
    return {
      '@type': 'api:SchemaCheckFailure',
      'api:witnesses': witnesses,
    }
  },
  woqlSchemaCheckFailure (witnesses) {
    return {
      '@type': 'api:WOQLSchemaCheckFailure',
      'api:witnesses': witnesses,
    }
  },
  documentNotFound (id, document) {
    const result = {
      '@type': 'api:DocumentNotFound',
      'api:document_id': id,
    }
    if (util.isDefined(document)) {
      result['api:document'] = document
    }
    return result
  },
  subdocumentKeyMissing (document) {
    return {
      '@type': 'api:SubdocumentKeyMissing',
      'api:document': document,
    }
  },
  documentKeyNotObject (document) {
    return {
      '@type': 'api:DocumentKeyNotObject',
      'api:key_value': document['@key'],
      'api:document': document,
    }
  },
  documentKeyTypeMissing (document) {
    return {
      '@type': 'api:DocumentKeyTypeMissing',
      'api:key': JSON.stringify(document['@key']),
      'api:document': document,
    }
  },
  documentKeyTypeUnknown (schema) {
    return {
      '@type': 'api:DocumentKeyTypeUnknown',
      'api:key_type': schema['@key']['@type'],
      'api:document': schema,
    }
  },
  unexpectedBooleanValue (value, expectedType) {
    return {
      '@type': 'api:UnexpectedBooleanValue',
      'api:value': value,
      'api:expected_type': expectedType,
    }
  },
  keyMissingFields (schema) {
    return {
      '@type': 'api:KeyMissingFields',
      'api:key_type': schema['@key']['@type'],
      'api:document': schema,
    }
  },
  keyFieldsNotAnArray (schema) {
    return {
      '@type': 'api:KeyFieldsNotAnArray',
      'api:fields': schema['@key']['@fields'],
      'api:document': schema,
    }
  },
  keyFieldsIsEmpty (schema) {
    return {
      '@type': 'api:KeyFieldsIsEmpty',
      'api:document': schema,
    }
  },
  documentInsertionReservedName (name) {
    return {
      '@type': 'api:DocumentInsertionReservedName',
      'api:id': name,
    }
  },
  invalidJSONDocumentId (id) {
    return {
      '@type': 'api:DocumentInsertionInvalidJSONDocumentId',
      'api:id': id,
    }
  },
  missingTargets: {
    '@type': 'api:MissingTargets',
  },
  queryMissingType: {
    '@type': 'api:QueryMissingType',
  },
  sameDocumentIdsMutatedInOneTransaction (ids) {
    return {
      '@type': 'api:SameDocumentIdsMutatedInOneTransaction',
      'api:duplicate_ids': ids,
    }
  },
  noContextFoundInSchema: {
    '@type': 'api:NoContextFoundInSchema',
  },
  badDataVersion (value) {
    return {
      '@type': 'api:BadDataVersion',
      'api:data_version': value,
    }
  },
  dataVersionMismatch (requested, actual) {
    return {
      '@type': 'api:DataVersionMismatch',
      'api:requested_data_version': requested,
      'api:actual_data_version': actual,
    }
  },
  badDescriptorPath (descriptor) {
    return {
      '@type': 'api:BadDescriptorPath',
      'api:descriptor': descriptor,
    }
  },
  unresolvableDescriptor (descriptor) {
    return {
      '@type': 'api:UnresolvableAbsoluteDescriptor',
      'api:absolute_descriptor': descriptor,
    }
  },
  badGraphDescriptor (descriptor) {
    return {
      '@type': 'api:BadAbsoluteGraphDescriptor',
      'api:absolute_graph_descriptor': descriptor,
    }
  },
  badOriginDescriptor (origin) {
    return {
      '@type': 'api:BadOriginAbsoluteDescriptor',
      'api:absolute_descriptor': origin,
    }
  },
  badTargetDescriptor (target) {
    return {
      '@type': 'api:BadTargetAbsoluteDescriptor',
      'api:absolute_descriptor': target,
    }
  },
  unknownOriginDatabase (orgName, dbName) {
    return {
      '@type': 'api:UnknownOriginDatabase',
      'api:organization_name': orgName,
      'api:database_name': dbName,
    }
  },
  notASourceBranchDescriptor (origin) {
    return {
      '@type': 'api:NotASourceBranchDescriptorError',
      'api:absolute_descriptor': origin,
    }
  },
  branchExists (name) {
    return {
      '@type': 'api:BranchExistsError',
      'api:branch_name': name,
    }
  },
  remoteDoesNotExist (name) {
    return {
      '@type': 'api:RemoteDoesNotExist',
      'api:remote_name': name,
    }
  },
  badCast (type, value) {
    return {
      '@type': 'api:BadCast',
      'api:type': type,
      'api:value': value,
    }
  },
  missingFile (fileName) {
    return {
      '@type': 'api:MissingFile',
      'api:file_name': fileName,
    }
  },
  httpRequestFailedFetch (url) {
    return {
      '@type': 'api:HttpRequestFailedFetch',
      'api:url': url,
    }
  },
  httpRequestFailedBadUrl (url) {
    return {
      '@type': 'api:HttpRequestFailedBadUrl',
      'api:url': url,
    }
  },
  httpRequestFailedSocketError (message) {
    return {
      '@type': 'api:HttpRequestFailedSocketError',
    }
  },
}
