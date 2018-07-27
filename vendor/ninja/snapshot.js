

var child = require('child_process')
var os = require('os')

child.execSync(`./configure.py --bootstrap`, {stdio:[0,1,2]})
child.execSync(`strip ninja`, {stdio:[0,1,2]})

var ext = os.platform()
child.execSync(`mv ./ninja ./snapshot/ninja.${ext}`, {stdio:[0,1,2]})
