open Js_dict

let obj (): 'a t = Obj.magic [%obj { foo = 43; bar = 86 }]

let suites = Mt.[
  "empty", (fun _ ->
    Eq([||], keys (empty ())));
  "get", (fun _ ->
    Eq(Some 43, get (obj ()) "foo"));
  "get - property not in object", (fun _ ->
    Eq(None, get (obj ()) "baz"));
  "unsafe_get", (fun _ ->
    Eq(43, unsafeGet (obj ()) "foo"));
  "set", (fun _ ->
    let o = obj () in 
    set o "foo" 36;
    Eq(Some 36, get o "foo"));
  "keys", (fun _ ->
    Eq([| "foo"; "bar" |], keys (obj ())));
  "entries", (fun _ ->
    Eq([| ("foo", 43); ("bar", 86) |], entries (obj ())));
  "values", (fun _ ->
    Eq([| 43; 86 |], values (obj ())));
  "fromList - []", (fun _ ->
    Eq(empty (), fromList []));
  "fromList", (fun _ ->
    Eq([|("x", 23); ("y", 46)|], fromList [("x", 23); ("y", 46)] |> entries));
  "fromArray - []", (fun _ ->
    Eq(empty (), fromArray [||]));
  "fromArray", (fun _ ->
    Eq([|("x", 23); ("y", 46)|], fromArray [|("x", 23); ("y", 46)|] |> entries));
  "map", (fun _ ->
    Eq( [%obj { foo = "43"; bar = "86" }] |> Obj.magic,
        map string_of_int (obj ())))
]
;; Mt.from_pair_suites __FILE__ suites
