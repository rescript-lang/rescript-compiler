

var set = new Set ()

for(var i = 0; i < 1000000; ++ i ){
    set.add(i)
}

for(var i = 0; i < 1000000; ++ i ){
    set.has(i)
}