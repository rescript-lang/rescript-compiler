var fs = require('fs')
var path = require('path')
var version = JSON.parse(fs.readFileSync('../package.json','utf8')).version

fs.writeFileSync(
    path.join(__dirname,'common', 'bs_version.ml'),
    "let version = \"" + version + "\"\n",'utf8')