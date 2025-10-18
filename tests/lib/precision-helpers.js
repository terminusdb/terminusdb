const Decimal = require('decimal.js')

/**
 * Precision Helpers for Testing
 *
 * Based on patterns from:
 * - tests/test/decimal-precision.js (lines 79-87)
 * - tests/test/graphql.js (line 735)
 * - docs/JSON_SERIALIZATION_RULES.md (lines 351-372)
 *
 * Key Insight: Extract numeric values from raw JSON text BEFORE JSON.parse()
 * to avoid IEEE 754 precision loss.
 */

/**
 * Extract numeric value from response text using regex
 *
 * Pattern from decimal-precision.js line 80:
 * const value20Raw = response.text.match(/"value20digits"\s*:\s*([0-9.eE+-]+)/)[1]
 *
 * @param {string} responseText - Raw HTTP response text
 * @param {string} fieldName - JSON field name to extract
 * @returns {string|null} Numeric value as string, or null if not found
 *
 * @example
 * const response = await document.get(agent, { body: { id: 'Product/1' } })
 * const priceStr = extractNumericValue(response.text, 'price')
 * const price = new Decimal(priceStr)  // Full precision preserved!
 */
function extractNumericValue (responseText, fieldName) {
  // Escape special regex characters in field name
  const escapedField = fieldName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')

  // Match pattern: "fieldName": 123.456
  const regex = new RegExp(`"${escapedField}"\\s*:\\s*([0-9.eE+-]+)`)
  const match = responseText.match(regex)

  return match ? match[1] : null
}

/**
 * Extract all numeric values from response text
 *
 * @param {string} responseText - Raw HTTP response text
 * @param {string[]} fieldNames - Array of field names to extract
 * @returns {Object<string, string>} Map of field name to numeric string value
 *
 * @example
 * const values = extractNumericValues(response.text, ['price', 'taxRate', 'quantity'])
 * // { price: '19.99', taxRate: '0.075', quantity: '3.5' }
 *
 * const price = new Decimal(values.price)
 */
function extractNumericValues (responseText, fieldNames) {
  const result = {}

  for (const fieldName of fieldNames) {
    const value = extractNumericValue(responseText, fieldName)
    if (value !== null) {
      result[fieldName] = value
    }
  }

  return result
}

/**
 * Helper to extract and convert WOQL result value
 *
 * @param {object} response - WOQL response (unverified)
 * @param {string} variableName - Result variable name (default: 'Result')
 * @returns {Decimal} Result value as Decimal
 *
 * @example
 * const response = await woql.post(agent, query).unverified()
 * const result = extractWOQLValue(response)
 * expect(result.equals(new Decimal('0.3'))).to.be.true
 */
function extractWOQLValue (response, variableName = 'Result') {
  // Pattern from decimal-precision.js line 148-155
  const parsed = JSON.parse(response.text)
  const binding = parsed.bindings[0][variableName]

  // Try to extract from raw text first for precision
  const regex = new RegExp(`"${variableName}"[^}]*"@value"\\s*:\\s*([0-9.eE+-]+)`)
  const match = response.text.match(regex)

  if (match) {
    return new Decimal(match[1])
  }

  // Fallback: use parsed value (may have precision loss)
  if (binding && '@value' in binding) {
    return new Decimal(binding['@value'].toString())
  }

  throw new Error(`Could not extract value for variable: ${variableName}`)
}

module.exports = {
  extractNumericValue,
  extractNumericValues,
  extractWOQLValue,
}
