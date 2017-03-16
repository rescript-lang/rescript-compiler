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
  let json = 
    Js.Json.number_of_int 0xAFAFAFAF
    |> Js.Json.to_string |> Js.Json.parse 
  in 
  let ty, x = Js.Json.reify_type json in
  match ty with
  | Js.Json.Number -> eq __LOC__ (int_of_float x) 0xAFAFAFAF
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

(* Check that the given json value is an array and that its element 
 * a position [i] is equal to both the [kind] and [expected] value *)
let eq_at_i 
      (type a) 
      (loc:string)
      (json:Js_json.t) 
      (i:int) 
      (kind:a Js.Json.kind) 
      (expected:a) : unit = 

  let ty, x = Js.Json.reify_type json in 
  match ty with
  | Js.Json.Array -> 
    let ty, a1 = Js.Json.reify_type x.(i) in 
    begin match ty with
    | kind' when kind' = kind ->
      eq loc a1 expected
    | _ -> false_ loc 
    end
  | _ -> false_ loc

let () = 
  let json = 
    [| "string 0"; "string 1"; "string 2" |]
    |> Array.map Js.Json.string
    |> Js.Json.array_
    |> Js.Json.to_string
    |> Js.Json.parse 
  in 
  eq_at_i __LOC__ json 0 Js.Json.String "string 0";
  eq_at_i __LOC__ json 1 Js.Json.String "string 1";
  eq_at_i __LOC__ json 2 Js.Json.String "string 2";
  ()

let () = 
  let json = 
    [| "string 0"; "string 1"; "string 2" |]
    |> Js.Json.string_array
    |> Js.Json.to_string
    |> Js.Json.parse 
  in 
  eq_at_i __LOC__ json 0 Js.Json.String "string 0";
  eq_at_i __LOC__ json 1 Js.Json.String "string 1";
  eq_at_i __LOC__ json 2 Js.Json.String "string 2";
  ()

let () = 
  let a = [| 1.0000001; 10000000000.1; 123.0 |] in
  let json = 
    a  
    |> Js.Json.number_array
    |> Js.Json.to_string
    |> Js.Json.parse 
  in 
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 Js.Json.Number a.(0);
  eq_at_i __LOC__ json 1 Js.Json.Number a.(1);
  eq_at_i __LOC__ json 2 Js.Json.Number a.(2);
  ()

let () = 
  let a = [| 0; 0xAFAFAFAF; 0xF000AABB|] in
  let json = 
    a  
    |> Js.Json.int_array
    |> Js.Json.to_string
    |> Js.Json.parse 
  in 
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 Js.Json.Number (float_of_int a.(0));
  eq_at_i __LOC__ json 1 Js.Json.Number (float_of_int a.(1));
  eq_at_i __LOC__ json 2 Js.Json.Number (float_of_int a.(2));
  ()

let () = 
  let a = [| true; false; true |] in
  let json = 
    a  
    |> Js.Json.boolean_array
    |> Js.Json.to_string
    |> Js.Json.parse 
  in 
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 Js.Json.Boolean (Js_boolean.to_js_boolean a.(0));
  eq_at_i __LOC__ json 1 Js.Json.Boolean (Js_boolean.to_js_boolean a.(1));
  eq_at_i __LOC__ json 2 Js.Json.Boolean (Js_boolean.to_js_boolean a.(2));
  ()

let () = Mt.from_pair_suites __FILE__ !suites
