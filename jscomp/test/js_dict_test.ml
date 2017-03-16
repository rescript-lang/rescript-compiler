open Js_dict

let obj (): 'a t = Obj.magic [%obj { foo = 43; bar = "baz" }]

let suites = Mt.[
  "get", (fun _ ->
    Eq(Some 43, get (obj ()) "foo"));
  "get - property not in object", (fun _ ->
    Eq(None, get (obj ()) "baz"));
  "unsafe_get", (fun _ ->
    Eq(43, unsafeGet (obj ()) "foo"));
  "exnGet - raise correct exn", (fun _ ->
    try 
      let _ = exnGet (obj ()) "baz" in 
      Ok false 
    with
    | (Invalid_argument _) -> Ok true 
    | _ -> Ok false
  ); 
  "exnGet - valid access", (fun _ ->
    try 
      Eq (43, exnGet (obj ()) "foo")
    with
    | _ -> Ok false
  ); 
  "set", (fun _ ->
    let o = obj () in 
    set o "foo" 36;
    Eq(Some 36, get o "foo"));
  "keys", (fun _ ->
    Eq([| "foo"; "bar" |], keys (obj ())));
  "empty", (fun _ ->
    Eq([||], keys (empty ())));
]
;; Mt.from_pair_suites __FILE__ suites
