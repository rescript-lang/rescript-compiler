
//@ts-check
var fs = require('fs')
var path = require('path')
function* range(n) {
    for (var i = 0; i < n; ++i) {
        yield i
    }
}

var constructors = (n) => {
    return [...range(n)].map((x => `| A${x} \n`)).reduce((x, y) => x + y)
}
var matches = (n) => {
    return [...range(n)].map((x => `| A${x} -> ${x} \n `)).reduce((x, y) => x + y)
}
var xs = (n) => {
    return [...range(n)].map((x => `| A${x} -> "A${x}" \n `)).reduce((x, y) => x + y)
}
var code = (n) => {
    var content = `type t = \n ${constructors(n)}`
    var match = `let to_enum = function\n ${matches(n)}`
    var to_string = 
        `let to_string = function\n ${xs(n)}`
    return content + match + to_string
}

fs.writeFileSync(path.join(__dirname,'..','jscomp','test','big_enum.ml'),
    code(300),'utf8')
