include Js_promise

/** Type-safe t-first then */
let then: (promise<'a>, 'a => promise<'b>) => promise<'b> = %raw(`
  function(p, cont) {
    Promise.resolve(p).then(cont)
  }
  `)

/** Type-safe t-first catch */
let catch: (promise<'a>, error => promise<'a>) => promise<'a> = %raw(`
    function(p, cont) {
      Promise.resolve(p).catch(cont)
    }
    `)

/** Old syntax, can use then instead */
let then_ = then
