

var Immutable = require('immutable')

// var Set = Immutable.OrderedSet
var Set = Immutable.Set
var count = 1000000 

function should(b){
    if (!b){
        throw new Error("impossible")
    }
}

function bench(){
    var m = new Set()
    for (var i = 0 ; i < count + 1; ++ i){
        m = m.add(i)
    }
    for (var j = 0; j < count + 1; ++ j){
        should(m.has(j))
    }
    for (var k = 0; k < count + 1; ++k) {
        m = m.delete(k)
    }
    should(m.size === 0)
}

console.time('start')
bench()
console.timeEnd('start')
