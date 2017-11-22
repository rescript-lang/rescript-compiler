
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

var polyConstructors = (n) => {
    return [...range(n)].map((x => `| \`variant${x} \n`)).reduce((x, y) => x + y)
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

var polyCode = (n) =>{
    var content = `type t = [ \n ${polyConstructors(n)}\n ] [@@bs.deriving jsConverter] `
    var eq = `
        let eq (x : t option) (y: t option) = 
            match x with 
            | Some x -> 
                (match y with None -> false | Some y -> x = y)
            | None -> y = None     
    `
    var assertions = 
        [...range(n)].map(x => `\n;;assert (tToJs \`variant${x} = "variant${x}")`).reduce((x,y)=> x +y)
    var assertions2 =         
        [...range(n)].map(x => `\n;;assert (eq (tFromJs "variant${x}")  (Some \`variant${x}))`).reduce((x,y)=> x +y)
    var assertions3  = 
        `\n;;assert (eq (tFromJs "xx") None) \n`   
    return content  + eq +  assertions  + assertions2 + assertions3
}
var run = () => {
    fs.writeFileSync(path.join(__dirname, '..', 'jscomp', 'test', 'big_enum.ml'),
        code(300), 'utf8')
}

var runPol = () =>{
    fs.writeFileSync(path.join(__dirname,'..','jscomp','test', 'big_polyvar_test.ml'),
    polyCode(300)
    )
} 

runPol()