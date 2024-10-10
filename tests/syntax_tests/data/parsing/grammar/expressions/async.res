let greetUser = async (userId) => {
  let name = await getUserName(. userId)  
  "Hello " ++ name ++ "!"
}

async () => 123

let fetch = {
    async (. url) => browserFetch(. url)
}

let fetch2 = {
    async (. url) => browserFetch(. url)
    async (. url) => browserFetch2(. url)
}

// don't parse async es6 arrow
let async = {
    let f = async()
    ()->async
    async()
    async.async

    {async: async[async]}

    result->async->mapAsync(a => doStuff(a))
}

let f = isPositive ? (async (a, b) : int => a + b) : async (c, d) : int => c - d

let foo = async(~a=34)
let bar = async(~a)=>a+1

let ex1 = await 3 + await 4
let ex2 = await 3 ** await 4
let ex3 = await foo->bar(~arg)
let ex4 = await foo.bar.baz
