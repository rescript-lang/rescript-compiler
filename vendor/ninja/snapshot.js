

var child = require('child_process')
var os = require('os')
var ext = os.platform()

if(ext === 'win32')
{
    child.exec("c:\\Python27\\python.exe configure.py --bootstrap")

} else {
    child.execSync(`./configure.py --bootstrap`, {stdio:[0,1,2]})
    child.execSync(`strip ninja`, {stdio:[0,1,2]})    
    child.execSync(`mv ./ninja ./snapshot/ninja.${ext}`, {stdio:[0,1,2]})
    
}
