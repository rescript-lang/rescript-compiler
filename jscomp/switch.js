#!/usr/bin/env node
var fs = require('fs')
var path = require('path')


var clean = require('./cleanlib.js')
function toggleMakefile() {
    var filename = path.join(__dirname,'Makefile.shared')
    var line = fs.readFileSync(filename,'utf8')
    var toggled = (line.split('\n').map(x => {
        if (x.startsWith('STDLIB')) {
            return '# ' + x
        } else if (x.startsWith('# STDLIB')) {
            return x.substring(2)
        }
        return x
    }).join('\n'))

    fs.writeFileSync(filename, toggled, 'utf8')
}
function toggleSharedMakefile(){
    var filename = path.join(__dirname, 'test','Makefile')
    var line = fs.readFileSync(filename,'utf8')
    var toggled = (line.split('\n').map(x => {
        if(x.startsWith('	# ocaml_typedtree_test')){
            return '	ocaml_typedtree_test'
        } else {
            if (x.startsWith('	ocaml_typedtree_test')){
                return '	# ocaml_typedtree_test'
            }
        }
        return x 
    })).join('\n')
    fs.writeFileSync(filename,toggled,'utf8')
}


toggleMakefile()
toggleSharedMakefile()
clean.run()


