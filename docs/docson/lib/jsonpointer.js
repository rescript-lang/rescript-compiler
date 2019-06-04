/**
 * @author Alexey Kuzmin <alex.s.kuzmin@gmail.com>
 * @fileoverview JavaScript implementation of JSON Pointer.
 * @see http://tools.ietf.org/html/rfc6901
 */



;(function() {
  'use strict';

  /**
   * List of special characters and their escape sequences.
   * Special characters will be unescaped in order they are listed.
   * Section 3 of spec.
   * @type {Array.<Array.<string>>}
   * @const
   */
  var SPECIAL_CHARACTERS = [
    ['/', '~1'],
    ['~', '~0']
  ];


  /**
   * Tokens' separator in JSON pointer string.
   * Section 3 of spec.
   * @type {string}
   * @const
   */
  var TOKENS_SEPARATOR = '/';


  /**
   * Prefix for error messages.
   * @type {string}
   * @const
   */
  var ERROR_MESSAGE_PREFIX = 'JSON Pointer: ';


  /**
   * Validates non-empty pointer string.
   * @type {RegExp}
   * @const
   */
  var NON_EMPTY_POINTER_REGEXP = /(\/[^\/]*)+/;


  /**
   * List of error messages.
   * Please keep it in alphabetical order.
   * @enum {string}
   */
  var ErrorMessage = {
    HYPHEN_IS_NOT_SUPPORTED_IN_ARRAY_CONTEXT:
        'Implementation does not support "-" token for arrays.',
    INVALID_DOCUMENT: 'JSON document is not valid.',
    INVALID_DOCUMENT_TYPE: 'JSON document must be a string or object.',
    INVALID_POINTER: 'Pointer is not valid.',
    NON_NUMBER_TOKEN_IN_ARRAY_CONTEXT:
        'Non-number tokens cannot be used in array context.',
    TOKEN_WITH_LEADING_ZERO_IN_ARRAY_CONTEXT:
        'Token with leading zero cannot be used in array context.'
  };


  /**
   * Returns |target| object's value pointed by |opt_pointer|, returns undefined
   * if |opt_pointer| points to non-existing value.
   * If pointer is not provided, validates first argument and returns
   * evaluator function that takes pointer as argument.
   * @param {(string|Object|Array)} target Evaluation target.
   * @param {string=} opt_pointer JSON Pointer string.
   * @returns {*} Some value.
   */
  function getPointedValue(target, opt_pointer) {
    // .get() method implementation.

    // First argument must be either string or object.
    if (isString(target)) {

      // If string it must be valid JSON document.
      try {
        // Let's try to parse it as JSON.
        target = JSON.parse(target);
      }
      catch (e) {
        // If parsing failed, an exception will be thrown.
        throw getError(ErrorMessage.INVALID_DOCUMENT);
      }
    }
    else if (!isObject(target)) {
      // If not object or string, an exception will be thrown.
      throw getError(ErrorMessage.INVALID_DOCUMENT_TYPE);
    }

    // |target| is already parsed, let's create evaluator function for it.
    var evaluator = createPointerEvaluator(target);

    if (isUndefined(opt_pointer)) {
      // If pointer was not provided, return evaluator function.
      return evaluator;
    }
    else {
      // If pointer is provided, return evaluation result.
      return evaluator(opt_pointer);
    }
  }


  /**
   * Returns function that takes JSON Pointer as single argument
   * and evaluates it in given |target| context.
   * Returned function throws an exception if pointer is not valid
   * or any error occurs during evaluation.
   * @param {*} target Evaluation target.
   * @returns {Function}
   */
  function createPointerEvaluator(target) {

    // Use cache to store already received values.
    var cache = {};

    return function(pointer) {

      if (!isValidJSONPointer(pointer)) {
        // If it's not, an exception will be thrown.
        throw getError(ErrorMessage.INVALID_POINTER);
      }

      // First, look up in the cache.
      if (cache.hasOwnProperty(pointer)) {
        // If cache entry exists, return it's value.
        return cache[pointer];
      }

      // Now, when all arguments are valid, we can start evaluation.
      // First of all, let's convert JSON pointer string to tokens list.
      var tokensList = parsePointer(pointer);
      var token;
      var value = target;

      // Evaluation will be continued till tokens list is not empty
      // and returned value is not an undefined.
      while (!isUndefined(value) && !isUndefined(token = tokensList.pop())) {
        // Let's evaluate token in current context.
        // `getValue()` might throw an exception, but we won't handle it.
        value = getValue(value, token);
      }

      // Pointer evaluation is done, save value in the cache and return it.
      cache[pointer] = value;
      return value;
    };
  }


  /**
   * Returns true if given |pointer| is valid, returns false otherwise.
   * @param {!string} pointer
   * @returns {boolean} Whether pointer is valid.
   */
  function isValidJSONPointer(pointer) {
    // Validates JSON pointer string.

    if (!isString(pointer)) {
      // If it's not a string, it obviously is not valid.
      return false;
    }

    if ('' === pointer) {
      // If it is string and is an empty string, it's valid.
      return true;
    }

    // If it is non-empty string, it must match spec defined format.
    // Check Section 3 of specification for concrete syntax.
    return NON_EMPTY_POINTER_REGEXP.test(pointer);
  }


  /**
   * Returns tokens list for given |pointer|. List is reversed, e.g.
   *     '/simple/path' -> ['path', 'simple']
   * @param {!string} pointer JSON pointer string.
   * @returns {Array} List of tokens.
   */
  function parsePointer(pointer) {
    // Converts JSON pointer string into tokens list.

    // Let's split pointer string by tokens' separator character.
    // Also we will reverse resulting array to simplify it's further usage.
    var tokens = pointer.split(TOKENS_SEPARATOR).reverse();

    // Last item in resulting array is always an empty string,
    // we don't need it, let's remove it.
    tokens.pop();

    // Now tokens' array is ready to use, let's return it.
    return tokens;
  }


  /**
   * Decodes all escape sequences in given |rawReferenceToken|.
   * @param {!string} rawReferenceToken
   * @returns {string} Unescaped reference token.
   */
  function unescapeReferenceToken(rawReferenceToken) {
    // Unescapes reference token. See Section 3 of specification.

    var referenceToken = rawReferenceToken;
    var character;
    var escapeSequence;
    var replaceRegExp;

    // Order of unescaping does matter.
    // That's why an array is used here and not hash.
    SPECIAL_CHARACTERS.forEach(function(pair) {
      character = pair[0];
      escapeSequence = pair[1];
      replaceRegExp = new RegExp(escapeSequence, 'g');
      referenceToken = referenceToken.replace(replaceRegExp, character);
    });

    return referenceToken;
  }


  /**
   * Returns value pointed by |token| in evaluation |context|.
   * Throws an exception if any error occurs.
   * @param {*} context Current evaluation context.
   * @param {!string} token Unescaped reference token.
   * @returns {*} Some value or undefined if value if not found.
   */
  function getValue(context, token) {
    // Reference token evaluation. See Section 4 of spec.

    // First of all we should unescape all special characters in token.
    token = unescapeReferenceToken(token);

    // Further actions depend of context of evaluation.

    if (isArray(context)) {
      // In array context there are more strict requirements
      // for token value.

      if ('-' === token) {
        // Token cannot be a "-" character,
        // it has no sense in current implementation.
        throw getError(ErrorMessage.HYPHEN_IS_NOT_SUPPORTED_IN_ARRAY_CONTEXT);
      }
      if (!isNumber(token)) {
        // Token cannot be non-number.
        throw getError(ErrorMessage.NON_NUMBER_TOKEN_IN_ARRAY_CONTEXT);
      }
      if (token.length > 1 && '0' === token[0]) {
        // Token cannot be non-zero number with leading zero.
        throw getError(ErrorMessage.TOKEN_WITH_LEADING_ZERO_IN_ARRAY_CONTEXT);
      }
      // If all conditions are met, simply return element
      // with token's value index.
      // It might be undefined, but it's ok.
      return context[token];
    }

    if (isObject(context)) {
      // In object context we can simply return element w/ key equal to token.
      // It might be undefined, but it's ok.
      return context[token];
    }

    // If context is not an array or an object,
    // token evaluation is not possible.
    // This is the expected situation and so we won't throw an error,
    // undefined value is perfectly suitable here.
    return;
  }


  /**
   * Returns Error instance for throwing.
   * @param {string} message Error message.
   * @returns {Error}
   */
  function getError(message) {
    return new Error(ERROR_MESSAGE_PREFIX + message);
  }


  function isObject(o) {
    return 'object' === typeof o && null !== o;
  }


  function isArray(a) {
    return Array.isArray(a);
  }


  function isNumber(n) {
    return !isNaN(Number(n));
  }


  function isString(s) {
    return 'string' === typeof s || s instanceof String;
  }


  function isUndefined(v) {
    return 'undefined' === typeof v;
  }


  // Let's expose API to the world.

  var jsonpointer = {
    get: getPointedValue
  };

  if ('object' === typeof exports) {
    // If `exports` is an object, we are in Node.js context.
    // We are supposed to act as Node.js package.
    module.exports = jsonpointer;
  } else if ('function' === typeof define && define.amd) {
    // If there is global function `define()` and `define.amd` is defined,
    // we are supposed to act as AMD module.
    define(function() {
      return jsonpointer;
    });
  } else {
    // Last resort.
    // Let's create global `jsonpointer` object.
    this.jsonpointer = jsonpointer;
  }

}).call((function() {
  'use strict';
  return (typeof window !== 'undefined' ? window : global);
})());
