var Immutable = require('immutable')

var set = Immutable.Set() 

for(var i = 0; i <= 2000000; ++i){
    set = set.add(i);
}
for(var i = 0; i <= 2000000; ++i){
    if (!set.has(i)){
        console.log("impossible")
    }
}
for(var i = 0; i <= 2000000;++i){
    set = set.delete(i)
}

if(set.size!==0){
    console.log('impossible')
}