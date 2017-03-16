let suites :  Mt.pair_suites ref  = ref []

let add_test = 
  let counter = ref 0 in
  fun loc test -> 
    incr counter; 
    let id = (loc ^ " id " ^ (string_of_int !counter)) in 
    suites := (id, test) :: ! suites

let eq loc x y = 
  add_test loc (fun _ -> Mt.Eq (x, y)) 

let false_ loc =
  add_test loc (fun _ -> Mt.Ok false)

let true_ loc =
  add_test loc (fun _ -> Mt.Ok true)

let () = 

  let v = Js.Json.parse {| { "x" : [1, 2, 3 ] } |} in

  add_test __LOC__ (fun _ -> 
    let ty, x = Js.Json.reify_type v in
    match (ty : _ Js.Json.kind) with
    | Js.Json.Object ->  (* compiler infer x : Js.Json.t Js.Dict.t *) 
      begin match Js.Dict.get x "x" with 
      | Some v -> 
        let ty2, x = Js.Json.reify_type v in
        begin match ty2 with 
        | Js.Json.Array ->  (* compiler infer x : Js.Json.t array *)
          x 
          |> Js.Array.forEach (fun  x -> 
              let (ty3, x) = Js.Json.reify_type x in 
              match ty3 with 
              | Js.Json.Number -> () 
              | _ -> assert false
          )
          |> (fun () -> Mt.Ok true) 
        | _ -> Mt.Ok false
        end
      | None -> 
        Mt.Ok false
      end
    | _ -> Mt.Ok false
  );

  eq __LOC__ (Js.Json.test v Object) true

let () = 
  let json = Js.Json.null |> Js.Json.to_string |> Js.Json.parse in 
  let ty, x = Js.Json.reify_type json in
  match ty with
  | Js.Json.Null -> true_ __LOC__
  | _ -> false_ __LOC__

let () = 
  let json = 
    Js.Json.string "test string" 
    |> Js.Json.to_string |> Js.Json.parse 
  in 
  let ty, x = Js.Json.reify_type json in
  match ty with
  | Js.Json.String -> eq __LOC__ x "test string"
  | _ -> false_ __LOC__

let () = 
  let json = 
    Js.Json.number 1.23456789
    |> Js.Json.to_string |> Js.Json.parse 
  in 
  let ty, x = Js.Json.reify_type json in
  match ty with
  | Js.Json.Number -> eq __LOC__ x 1.23456789
  | _ -> add_test __LOC__ (fun _ -> Mt.Ok false) 

let () = 
  let test v = 
    let json = 
        Js.Json.boolean v |> Js.Json.to_string |> Js.Json.parse 
    in 
    let ty, x = Js.Json.reify_type json in
    match ty with
    | Js.Json.Boolean -> eq __LOC__ (Js.to_bool x) v
    | _ -> false_ __LOC__
  in
  test true; 
  test false;
  ()
 
let option_get = function | None -> assert false | Some x -> x

let () = 
  let dict = Js_dict.empty  () in 
  Js_dict.set dict "a" (Js_json.string "test string"); 
  Js_dict.set dict "b" (Js_json.number 123.0); 

  let json = 
    dict |> Js.Json.object_ |> Js.Json.to_string |> Js.Json.parse 
  in

  (* Make sure parsed as Object *)
  let ty, x = Js.Json.reify_type json in
  match ty with
  | Js.Json.Object -> 

    (* Test field 'a' *)
    let ta, a = Js.Json.reify_type (option_get @@ Js_dict.get x "a") in 
    begin match ta with
    | Js.Json.String -> 
      if a <> "test string" 
      then false_ __LOC__
      else
        (* Test field 'b' *)
        let ty, b = Js.Json.reify_type (option_get @@ Js_dict.get x "b") in 
        begin match ty with
        | Js.Json.Number -> 
          add_test __LOC__ (fun _ -> Mt.Approx (123.0, b))
        | _ -> false_ __LOC__
        end 
    | _ -> false_ __LOC__
    end
  | _ -> false_ __LOC__

let () = Mt.from_pair_suites __FILE__ !suites
