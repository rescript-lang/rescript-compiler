

var process = require('process')

var arr = []
var count = 1000000
var sum = 0

function sum1() {
    console.time('add')
    for (var i = 0; i < count; ++i) {        
        arr.push([i, ""+i])
    }
    console.timeEnd('add')
    console.time('query')
    for (var i = 0; i < count; ++i) {
        sum += arr[i][0]
    }
    console.timeEnd('query')
    return sum
}

function sum2() {
    console.time('add')
    for (var i = 0; i < count; ++i) {
        arr.push({ key: i, value: ""+i })
    }
    console.timeEnd('add')
    console.time('query')
    for (var i = 0; i < count; ++i) {
        sum += arr[i].key
    }
    console.timeEnd('query')
    return sum
}

function point(x, y) {
    this.key = x
    this.value = y
}

function sum3() {
    console.time('add')
    for (var i = 0; i < count; ++i) {
        arr.push(new point(i, ""+i))
    }
    console.timeEnd('add')
    console.time('query')
    for (var i = 0; i < count; ++i) {
        sum += arr[i].key
    }
    console.timeEnd('query')
    return sum
}
var i = 2

if (process.argv[i] === "1") {
    console.log(sum1())
}
else if (process.argv[i] === "2"){
    console.log(sum2())
}else if(process.argv[i] === "3"){
    console.log(sum3())
}
console.log(process.memoryUsage())



// jscomp>node xx.js 1
// add: 160.560ms
// query: 52.166ms
// 499999500000
// { rss: 160428032,
//   heapTotal: 134868992,
//   heapUsed: 113233512,
//   external: 8236 }
// jscomp>node xx.js 2
// add: 175.138ms
// query: 41.904ms
// 499999500000
// { rss: 147386368,
//   heapTotal: 110227456,
//   heapUsed: 89490736,
//   external: 8236 }
// jscomp>node xx.js 3
// add: 180.078ms
// query: 46.473ms
// 499999500000
// { rss: 149192704,
//   heapTotal: 110227456,
//   heapUsed: 77898624,
//   external: 8236 }


// when value is of different type with key
// jscomp>node xx.js 1
// add: 269.281ms
// query: 144.137ms
// 499999500000
// { rss: 167034880,
//   heapTotal: 154619904,
//   heapUsed: 132656576,
//   external: 8236 }
// jscomp>node xx.js 2
// add: 271.878ms
// query: 69.863ms
// 499999500000
// { rss: 175353856,
//   heapTotal: 142733312,
//   heapUsed: 121749992,
//   external: 8236 }
// jscomp>node xx.js 3
// add: 248.261ms
// query: 89.188ms
// 499999500000
// { rss: 177520640,
//   heapTotal: 143257600,
//   heapUsed: 110167536,
//   external: 8236 }