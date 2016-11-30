var map  = new Map()

for(var i = 0; i < 1000000; ++ i ){
    map.set(i,i)
}

for(var i = 0; i < 1000000; ++ i ){
    map.has(i)
}
