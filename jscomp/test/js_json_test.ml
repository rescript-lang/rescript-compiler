
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let v = Js.Json.parse {| { "x" : [1, 2, 3 ] } |}


let () =
  let ty, x = Js.Json.reify_type v in
  match (ty : _ Js.Json.kind) with
  | Js.Json.Object ->  (* compiler infer x : Js.Json.t Js.Dict.t *) 
    begin match Js.Dict.get x "x" with 
    | Some v -> 
      let ty2, x = Js.Json.reify_type v in
      begin match ty2 with 
      | Js.Json.Array ->  (* compiler infer x : Js.Json.t array *)
        x |> Js.Array.forEach (fun  x -> 
            let (ty3, x) = Js.Json.reify_type x in 
            match ty3 with 
            | Js.Json.Number -> Js.log (x +. 0.)
            | _ -> assert false
          )
      | _ -> assert false
      end
    | None -> 
      assert false 
    end
  | _ -> assert false

(* parse & reify tests *)
let () =
  eq __LOC__  (Js.Json.test v Object) true

(* stringifyAny tests *)
let () = eq __LOC__ (Js.Json.stringifyAny [|1; 2; 3|]) (Some "[1,2,3]")

let () =
  eq
  __LOC__
  (Js.Json.stringifyAny [%bs.obj {foo = 1; bar = "hello"; baz = [%bs.obj {baaz = 10}]}])
  (Some {|{"foo":1,"bar":"hello","baz":{"baaz":10}}|})

let () = eq __LOC__ (Js.Json.stringifyAny Js.Null.empty) (Some "null")

let () = eq __LOC__ (Js.Json.stringifyAny Js.Undefined.empty) None


let () = Mt.from_pair_suites __FILE__ !suites
