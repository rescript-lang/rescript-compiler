<p align='right'>A <a href="http://www.swisspush.org">swisspush</a> project <a href="http://www.swisspush.org" border=0><img align="top"  src='https://1.gravatar.com/avatar/cf7292487846085732baf808def5685a?s=32'></a></p>
Docson
======

Documentation for your JSON types.

Give Docson a JSON schema and it will generate a [beautiful documentation](http://lbovet.github.io/docson/index.html#/docson/examples/example.json).

## Features
* [JSON schema](http://json-schema.org/) v4 keywords.
* Runs entirely in the browser.
* Render schema descriptions with markdown

## Installation

* Place the Docson distribution on the web server serving the schemas (to avoid cross-origin issues).

## Usage

* Open [index.html](http://lbovet.github.io/docson/index.html) and enter the schema path in the form field.
* Or give the schema path directly as hash parameter: [index.html#/docson/examples/example.json](http://lbovet.github.io/docson/index.html#/docson/examples/example.json)

Note that you can refer to a sub-schema by adding a json-pointer path as 'dollar-parameter': [index.html#/docson/examples/example.json$items](http://lbovet.github.io/docson/index.html#/docson/examples/example.json$items)

## Typson

You can directly reference your JSON types defined as TypeScript interfaces. If the path ends with `.ts`, Docson will use [Typson](https://github.com/lbovet/typson) to convert the Type Scripts to schema in order to generate the documentation.

For example, [index.html#/typson/example/invoice/line.ts$InvoiceLine](http://lbovet.github.io/docson/index.html#/typson/example/invoice/line.ts$InvoiceLine) is the documentation of [line.ts](https://github.com/lbovet/typson/blob/master/example/invoice/line.ts).

You need to install [Typson](https://github.com/lbovet/typson) by yourself on your server. It must be in a directory named `typson` located at the same level as the `docson` directory.

## Widget

To include a Docson schema documentations on any page (wiki, ...) without worrying about messing up with javascript libraries and cross-origin issues:

* Install Docson somewhere as described above.
* Place the following `script` tags in the including page, nothing else is needed:

```
<script src="http://somewhere/path-to-docson/widget.js" data-schema="/path-to-schema">
</script>
```

See the [widget example](http://jsfiddle.net/3kXu2/3/) on jsfiddle.

## Swagger

You can adapt [Swagger UI](https://github.com/wordnik/swagger-ui) to display Docson-generated model documentation instead of the builtin signatures.

See how it looks like in the [Swagger Docson example](http://lbovet.github.io/swagger-ui/dist/index.html)

In Swagger UI's `index.html`, include the [Swagger integration script after other script tags](https://github.com/lbovet/swagger-ui/blob/3f37722b03db6c48cc2a8460df26dda5f4d6f8e4/src/main/html/index.html#L19):
```
  <script src='/path-to-docson/docson-swagger.js' type='text/javascript'></script>
```

Also, you will need a patched version of [Swagger Client](https://github.com/lbovet/swagger-js/blob/models-exposed/lib/swagger.js) so that the raw json-schema model is visible from Docson. Either replace the `swagger.js` file in your Swagger UI disctribution or take it directly from github by replacing

```
   <script src='/lib/swagger.js' type='text/javascript'></script>
```

with 

```
  <script src='https://raw2.github.com/lbovet/swagger-js/models-exposed/lib/swagger.js' type='text/javascript'></script>
```

For a better layout of parameter models, you may [want to change the width of some elements](https://github.com/lbovet/swagger-ui/blob/3f37722b03db6c48cc2a8460df26dda5f4d6f8e4/src/main/html/index.html#L20-L27):

```
  <style>
      .swagger-ui-wrap {
          max-width: 1200px;
      }
      .swagger-ui-wrap .body-textarea {
          width: 200px;
      }
  </style>
```

## Integration

You can also integrate Docson in your application and use its javascript API:

```javascript
docson.doc(element, schema, ref)
```

* `element` is the element which will host the documentation. Either a DOM element (id or object) or jQuery element.
* `schema` is the URI or path to the schema or a string containing the schema source itself.
* `ref` is an optional json-pointer path to a sub-schema.

Examples:
* [Simple integration example](http://lbovet.github.io/docson/examples/example.html)
* [See it in action](http://lbovet.github.io/typson-demo/) with its buddy [typson](https://github.com/lbovet/typson).

## Limitations

* Mixing unrelated keywords can lead to unexpected results.

Not implemented:
* Non-primitive values in enums and default values
* dependencies, additionalItems, patternProperties

## Development

* [All tests](http://lbovet.github.io/docson/tests/test.html)

Please pull-request your failing schemas in the `tests/` folder and open an issue describing the expected result.

[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/lbovet/docson/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

