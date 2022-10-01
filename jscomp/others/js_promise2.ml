include Js_promise

let then_ : 'a promise -> ('a -> 'b promise) -> 'b promise =
  [%raw {|
  function(p, cont) {
    Promise.resolve(p).then(cont)
  }
  |}]

let catch_ : 'a promise -> (error -> 'a promise) -> 'a promise =
  [%raw
    {|
    function(p, cont) {
      Promise.resolve(p).catch(cont)
    }
    |}]
