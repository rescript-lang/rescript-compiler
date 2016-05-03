'use strict';
var child_process = require('child_process')
var fs = require('fs')

var copy_reg = /\(\*\s*Author:\s*Hongbo\s*Zhang\s*\*\)/
var utf8 = {encoding: 'utf8'}

var license_template = 
    fs.readFileSync('scripts/LICENSE.ml',utf8)

function handleFile(file){
  
    if(!fs.existsSync(file)){
	// console.log(file,'not exist')
    } else{
	var stat = fs.lstatSync(file);
	if (stat.isSymbolicLink()){
	    console.error(file, 'is symbol link')
	} else {
	    var contents = fs.readFileSync(file,utf8);

	    if (contents.includes('Author:')) {
		var x = copy_reg.exec(contents)
		if(x){
		    var valid_content = x.input.substr(x.index+x[0].length);
		    fs.writeFileSync(file, 
				     license_template + valid_content,utf8)
		}
		else{
		    console.error(file,'wrong format')
		}
	    }
	    else {
		console.log(file, 'no authors')
	    }
	}
    }
}

child_process.execSync('git ls-files *.ml *.mli', utf8)
    .split('\n')
    .filter(function(x){return x && !(x.includes('/test/')) && !( x.includes('/tools/')) && !(x.includes('/stdlib/')) && !(x.includes('/test_npm/'))})
    .forEach(handleFile)

