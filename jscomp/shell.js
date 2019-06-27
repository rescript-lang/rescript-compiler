var cp = require('child_process')

var output = cp.execSync('git branch',{encoding:'ascii'})
output.split('\n').forEach(x=>{
    let y = x.trim()
    if(Boolean(y)){
        let command = `git branch -d ${y}`
        try{
        cp.execSync(command,{encoding : 'ascii'})
        }catch(e){
            console.log(e.stderr)
        }
    }
})
debugger