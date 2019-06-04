
/*
 * Copyright 2013 Geraint Luff <http://geraintluff.github.io/tv4/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

define(function() {

    var highlight = {};

    var REGEX = {
        whitespace: /^([ \r\n\t]|&nbsp;)*/,
        number: /^-?[0-9]+(\.[0-9]+)?([eE][+\-]?[0-9]+)?/
    };

    function Highlighter(stringData) {
        this.remaining = stringData;
        this.html = "";
    }
    Highlighter.prototype = {
        unshift: function (next) {
            this.remaining = next + this.remaining;
        },
        next: function () {
            this.whitespace();
            return this.nextCharacter();
        },
        nextCharacter: function () {
            if (this.remaining.length == 0) {
                throw new Error("Unexpected end of input");
            }
            if (this.remaining[0] == "&") {
                var endIndex = this.remaining.indexOf(";") + 1;
                if (endIndex == -1) {
                    endIndex = 1;
                }
                var result = this.remaining.substring(0, endIndex);
                this.remaining = this.remaining.substring(endIndex);
                return result;
            }
            var result = this.remaining[0];
            this.remaining = this.remaining.substring(1);
            return result;
        },
        whitespace: function () {
            var ws = this.remaining.match(REGEX.whitespace)[0];
            this.html += ws;
            this.remaining = this.remaining.substring(ws.length);
        },
        highlightJson: function (keywords) {
            if (keywords != undefined) {
                this.html += keywords.wrapper[0];
            }
            this.whitespace();
            var next = this.next();
            if (next == "{") {
                this.highlightObject(keywords);
            } else if (next == '[') {
                this.highlightArray(keywords);
            } else if (next == '"' || next == "&quot;") {
                this.highlightString();
            } else if ((next + this.remaining).match(REGEX.number)) {
                var numberString = (next + this.remaining).match(REGEX.number)[0];
                this.html += '<span class="json-number">' + numberString + '</span>';
                this.remaining = this.remaining.substring(numberString.length - 1);
            } else if (next == "n" && this.remaining.substring(0, 3) == "ull") {
                this.remaining = this.remaining.substring(3);
                this.html += '<span class="json-null">null</span>';
            } else if (next == "t" && this.remaining.substring(0, 3) == "rue") {
                this.remaining = this.remaining.substring(3);
                this.html += '<span class="json-true">true</span>';
            } else if (next == "f" && this.remaining.substring(0, 4) == "alse") {
                this.remaining = this.remaining.substring(4);
                this.html += '<span class="json-false">false</span>';
            } else {
                this.html += next;
                this.highlightJson(keywords);
            }
            if (keywords != undefined) {
                this.html += keywords.wrapper[1];
            }
        },
        highlightObject: function (keywords) {
            this.html += '<span class="json-punctuation">{</span>';
            var next = this.next();
            while (next != "}") {
                if (next == '"' || next == "&quot;") {
                    var keyHtml = "";
                    next = this.next();
                    while (next != '"' && next != '&quot') {
                        if (next == "\\") {
                            keyHtml += next;
                            next = this.nextCharacter();
                        }
                        keyHtml += next;
                        next = this.next();
                    }
                    if (keywords != undefined && keywords.isKeyword(keyHtml)) {
                        this.html += '<span class="json-keyword">&quot;'
                            + keyHtml
                            + '&quot;</span>';
                    } else {
                        this.html += '<span class="json-object-key">&quot;'
                            + keyHtml
                            + '&quot;</span>';
                    }
                    next = this.next();
                    while (next != ":") {
                        this.html += next;
                        next = this.next();
                    }
                    this.html += '<span class="json-punctuation">:</span>';
                    var nextKeywords = null;
                    if (keywords != undefined) {
                        nextKeywords = keywords.forKey(keyHtml);
                    }
                    this.highlightJson(nextKeywords);
                    next = this.next();
                    if (next == ",") {
                        this.html += '<span class="json-punctuation">,</span>';
                        next = this.next();
                        continue;
                    } else while (next != "}") {
                        this.html += next;
                        next = this.next();
                    }
                } else {
                    this.html += next;
                    next = this.next();
                }
            }
            this.html += '<span class="json-punctuation">}</span>';
        },
        highlightArray: function (keywords) {
            this.html += '<span class="json-punctuation">[</span>';
            var next = this.next();
            var i = 0;
            while (next != "]") {
                this.unshift(next);
                this.highlightJson(keywords != undefined ? keywords.forItem(i) : null);
                next = this.next();
                if (next == ",") {
                    this.html += '<span class="json-punctuation">,</span>';
                    next = this.next();
                    i++;
                    continue;
                } else while (next != "]") {
                    this.html += next;
                    next = this.next();
                }
            }
            this.html += '<span class="json-punctuation">]</span>';
        },
        highlightString: function () {
            this.html += '<span class="json-punctuation">&quot;</span><span class="json-string">';
            next = this.next();
            while (next != '"' && next != '&quot') {
                if (next == "\\") {
                    this.html += next;
                    next = this.nextCharacter();
                }
                this.html += next;
                next = this.next();
            }
            this.html += '</span><span class="json-punctuation">&quot;</span>';
        }
    };

    function KeywordMap() {
    }
    KeywordMap.prototype = {
        wrapper: ["<span>", "</span>"],
        keywords: {},
        isKeyword: function (keyHtml) {
            return this.keywords[keyHtml] !== undefined;
        },
        forKey: function (keyHtml) {
            return this.keywords[keyHtml];
        },
        forItem: function (keyHtml) {
            return null;
        }
    };
    var schema = new KeywordMap();
    var schemaMedia = new KeywordMap();
    var mapToSchemas = new KeywordMap();
    var links = new KeywordMap();
    schema.keywords = {
        // from v3
        type: null,
        properties: mapToSchemas,
        patternProperties: mapToSchemas,
        additionalProperties: schema,
        items: schema,
        additionalItems: schema,
        required: null,
        dependencies: mapToSchemas,
        minimum: null,
        maximum: null,
        exclusiveMinimum: null,
        exclusiveMaximum: null,
        minItems: null,
        maxItems: null,
        uniqueItems: null,
        pattern: null,
        minLength: null,
        maxLength: null,
        "enum": null,
        "default": null,
        title: null,
        description: null,
        format: null,
        divisibleBy: null,
        disallow: schema,
        "extends": schema,
        "id": null,
        "$ref": null,
        "$schema": null,
        // from v4 core
        multipleOf: null,
        maxProperties: null,
        minProperties: null,
        allOf: schema,
        anyOf: schema,
        oneOf: schema,
        not: schema,
        definitions: mapToSchemas,
        // from v4 hyper-schema
        media: schemaMedia,
        links: links,
        pathStart: null,
        fragmentResolution: null
    };
    schema.forItem = function () {
        return schema;
    };
    schemaMedia.keywords = {
        binaryEncoding: null,
        type: null
    };
    mapToSchemas.wrapper = ['<span class="json-schema-map">', '</span>'];
    mapToSchemas.forKey = function () {
        return schema;
    };
    links.keywords = {
        rel: null,
        href:null,
        method: null,
        encType: null,
        pathStart: null,
        schema: schema,
        targetSchema: schema
    };
    links.forItem = function () {
        return links;
    };

    function highlightElement(element, keywords) {
        var highlighter = new Highlighter(element.innerHTML);
        try {
            highlighter.highlightJson(keywords);
        } catch (e) {
            throw e;
        }
        element.innerHTML = highlighter.html + highlighter.remaining;
    }

    if (document.getElementsByClassName == undefined) {
        document.getElementsByClassName = function(className)
        {
            var hasClassName = new RegExp("(?:^|\\s)" + className + "(?:$|\\s)");
            var allElements = document.getElementsByTagName("*");
            var results = [];

            var element;
            for (var i = 0; (element = allElements[i]) != null; i++) {
                var elementClass = element.className;
                if (elementClass && elementClass.indexOf(className) != -1 && hasClassName.test(elementClass))
                    results.push(element);
            }

            return results;
        }
    }

    highlight.highlightSchema = function(element) {
        highlightElement(element, schema);
    }

    return highlight;
});
