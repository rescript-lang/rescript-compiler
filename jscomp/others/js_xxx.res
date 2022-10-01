include Js_promise

let then: (promise<'a>, 'a => promise<'b>) => promise<'b> = %raw(`
  function(p, cont) {
    Promise.resolve(p).then(cont)
  }
  `)

let catch: (promise<'a>, error => promise<'a>) => promise<'a> = %raw(`
    function(p, cont) {
      Promise.resolve(p).catch(cont)
    }
    `)
