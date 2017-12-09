


var empty = new Map()

function bench() {
    for (var i = 0; i < 1000000; ++i) {
        empty.set( i, i)
    }
    for (var i = 0; i < 1000000; ++i) {
        if (!empty.has( i)) {
            throw "impossible"
        }
    }
}

console.time('start')
bench()
console.timeEnd('start')