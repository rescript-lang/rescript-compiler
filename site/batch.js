var fs = require('fs')

var p = require('child_process')

var path = require ('path')
fs
.readdirSync('.')
.filter(x=>x.endsWith(".md"))
.forEach(x=>{
    var command = `pandoc --from markdown_github --to asciidoc ${x} -o docs/${path.basename(x,'.md')}.adoc`;
    // console.log(command)
    p.execSync(command)
}
)
