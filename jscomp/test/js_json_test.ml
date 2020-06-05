let suites :  Mt.pair_suites ref  = ref []
module J = Js.Json

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

  let v = J.parseExn {| { "x" : [1, 2, 3 ] } |} in

  add_test __LOC__ (fun _ ->
    let ty = J.classify v in
    match ty with
    | J.JSONObject x ->  (* compiler infer x : J.t Js.Dict.t *)
      begin match Js.Dict.get x "x" with
      | Some v ->
        let ty2 = J.classify v in
        begin match ty2 with
        | J.JSONArray x ->  (* compiler infer x : J.t array *)
          x
          |. Js.Array2.forEach (fun  x ->
              let ty3 = J.classify x in
              match ty3 with
              | J.JSONNumber _ -> ()
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

  eq __LOC__ (J.test v Object) true

let () =
  let json = J.null |> J.stringify |> J.parseExn in
  let ty = J.classify json in
  match ty with
  | J.JSONNull -> true_ __LOC__
  | _ -> Js.log ty; false_ __LOC__

let () =
  let json =
    J.string "test string"
    |> J.stringify |> J.parseExn
  in
  let ty = J.classify json in
  match ty with
  | J.JSONString x -> eq __LOC__ x "test string"
  | _ -> false_ __LOC__

let () =
  let json =
    J.number 1.23456789
    |> J.stringify |> J.parseExn
  in
  let ty = J.classify json in
  match ty with
  | J.JSONNumber x -> eq __LOC__ x 1.23456789
  | _ -> add_test __LOC__ (fun _ -> Mt.Ok false)

let () =
  let json =
    J.number (float_of_int 0xAFAFAFAF)
    |> J.stringify |> J.parseExn
  in
  let ty = J.classify json in
  match ty with
  | J.JSONNumber x -> eq __LOC__ (int_of_float x) 0xAFAFAFAF
  | _ -> add_test __LOC__ (fun _ -> Mt.Ok false)

let () =
  let test v =
    let json =
        J.boolean v |> J.stringify |> J.parseExn
    in
    let ty = J.classify json in
    match ty with
    | J.JSONTrue  -> eq __LOC__ true v
    | J.JSONFalse  -> eq __LOC__ false v
    | _ -> false_ __LOC__
  in
  test true;
  test false;
  ()

let option_get = function | None -> assert false | Some x -> x

let () =
  let dict = Js_dict.empty  () in
  Js_dict.set dict "a" (J.string "test string");
  Js_dict.set dict "b" (J.number 123.0);

  let json =
    dict |> J.object_ |> J.stringify |> J.parseExn
  in

  (* Make sure parsed as Object *)
  let ty = J.classify json in
  match ty with
  | J.JSONObject x ->

    (* Test field 'a' *)
    let ta = J.classify (option_get @@ Js_dict.get x "a") in
    begin match ta with
    | J.JSONString a ->
      if a <> "test string"
      then false_ __LOC__
      else
        (* Test field 'b' *)
        let ty = J.classify (option_get @@ Js_dict.get x "b") in
        begin match ty with
        | J.JSONNumber b ->
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
      (json:J.t)
      (i:int)
      (kind:a J.kind)
      (expected:a) : unit =

  let ty = J.classify json in
  match ty with
  | J.JSONArray x ->
    let ty = J.classify x.(i) in
    begin match  kind with
    |  J.Boolean ->
      (match ty with
      | JSONTrue ->
        eq loc true expected

      | JSONFalse ->
        eq loc false expected
      | _ -> false_ loc)
    | J.Number ->
      (match ty with
      | JSONNumber f -> eq loc f expected
      | _ -> false_ loc
      )
    | J.Object  ->
      (match ty with
      | JSONObject f -> eq loc f expected
      | _ -> false_ loc
      )
    | J.Array ->
      (match ty with
      | JSONArray f -> eq loc f expected
      | _ -> false_ loc)
    | J.Null ->
      (match ty with
      | JSONNull -> true_ loc
      | _ -> false_ loc)
    | J.String ->
      (match ty with
      | JSONString f -> eq loc f expected
      | _ -> false_ loc
      )
    end
  | _ -> false_ loc

let () =
  let json =
    [| "string 0"; "string 1"; "string 2" |]
    |> Array.map J.string
    |> J.array
    |> J.stringify
    |> J.parseExn
  in
  eq_at_i __LOC__ json 0 J.String "string 0";
  eq_at_i __LOC__ json 1 J.String "string 1";
  eq_at_i __LOC__ json 2 J.String "string 2";
  ()

let () =
  let json =
    [| "string 0"; "string 1"; "string 2" |]
    |> J.stringArray
    |> J.stringify
    |> J.parseExn
  in
  eq_at_i __LOC__ json 0 J.String "string 0";
  eq_at_i __LOC__ json 1 J.String "string 1";
  eq_at_i __LOC__ json 2 J.String "string 2";
  ()

let () =
  let a = [| 1.0000001; 10000000000.1; 123.0 |] in
  let json =
    a
    |> J.numberArray
    |> J.stringify
    |> J.parseExn
  in
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 J.Number a.(0);
  eq_at_i __LOC__ json 1 J.Number a.(1);
  eq_at_i __LOC__ json 2 J.Number a.(2);
  ()

let () =
  let a = [| 0; 0xAFAFAFAF; 0xF000AABB|] in
  let json =
    a
    |> Array.map float_of_int
    |> J.numberArray
    |> J.stringify
    |> J.parseExn
  in
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 J.Number (float_of_int a.(0));
  eq_at_i __LOC__ json 1 J.Number (float_of_int a.(1));
  eq_at_i __LOC__ json 2 J.Number (float_of_int a.(2));
  ()

let () =
  let a = [| true; false; true |] in
  let json =
    a
    |> J.booleanArray
    |> J.stringify
    |> J.parseExn
  in
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 J.Boolean a.(0);
  eq_at_i __LOC__ json 1 J.Boolean a.(1);
  eq_at_i __LOC__ json 2 J.Boolean a.(2);
  ()

let () =
  let make_d s i =
    let d = Js_dict.empty() in
    Js_dict.set d "a" (J.string s);
    Js_dict.set d "b" (J.number (float_of_int i));
    d
  in

  let a = [| make_d "aaa" 123; make_d "bbb" 456 |] in
  let json =
    a
    |> J.objectArray
    |> J.stringify
    |> J.parseExn
  in

  let ty= J.classify json in
  match ty with
  | J.JSONArray x ->
    let ty= J.classify x.(1) in
    begin match ty with
    | J.JSONObject a1->
      let ty =  J.classify @@ option_get @@ Js_dict.get a1 "a" in
      begin match ty with
      | J.JSONString aValue -> eq __LOC__ aValue "bbb"
      | _ -> false_ __LOC__
      end
    | _ -> false_ __LOC__
    end
  | _ -> false_ __LOC__

let () =
  let invalid_json_str = "{{ A}" in
  try
    let _ = J.parseExn invalid_json_str in
    false_ __LOC__
  with
  | exn ->
    true_ __LOC__

(* stringifyAny tests *)

let () = eq __LOC__ (J.stringifyAny [|1; 2; 3|]) (Some "[1,2,3]")

let () =
  eq
  __LOC__
  (J.stringifyAny [%bs.obj {foo = 1; bar = "hello"; baz = [%bs.obj {baaz = 10}]}])
  (Some {|{"foo":1,"bar":"hello","baz":{"baaz":10}}|})

let () = eq __LOC__ (J.stringifyAny Js.Null.empty) (Some "null")

let () = eq __LOC__ (J.stringifyAny Js.Undefined.empty) None

let () =
  eq __LOC__
    (J.decodeString (J.string "test")) (Some "test");
  eq __LOC__
    (J.decodeString (J.boolean true)) None;
  eq __LOC__
    (J.decodeString (J.array [||])) None;
  eq __LOC__
    (J.decodeString J.null) None;
  eq __LOC__
    (J.decodeString (J.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__
    (J.decodeString (J.number 1.23)) None

let () =
  eq __LOC__
    (J.decodeNumber (J.string "test")) None;
  eq __LOC__
    (J.decodeNumber (J.boolean true)) None;
  eq __LOC__
    (J.decodeNumber (J.array [||])) None;
  eq __LOC__
    (J.decodeNumber J.null) None;
  eq __LOC__
    (J.decodeNumber (J.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__
    (J.decodeNumber (J.number 1.23)) (Some 1.23)

let () =
  eq __LOC__
    (J.decodeObject (J.string "test")) None;
  eq __LOC__
    (J.decodeObject (J.boolean true)) None;
  eq __LOC__
    (J.decodeObject (J.array [||])) None;
  eq __LOC__
    (J.decodeObject J.null) None;
  eq __LOC__
    (J.decodeObject (J.object_ @@ Js.Dict.empty ()))
    (Some (Js.Dict.empty ()));
  eq __LOC__
    (J.decodeObject (J.number 1.23)) None

let () =
  eq __LOC__
    (J.decodeArray (J.string "test")) None;
  eq __LOC__
    (J.decodeArray (J.boolean true)) None;
  eq __LOC__
    (J.decodeArray (J.array [||])) (Some [||]);
  eq __LOC__
    (J.decodeArray J.null) None;
  eq __LOC__
    (J.decodeArray (J.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__
    (J.decodeArray (J.number 1.23)) None

let () =
  eq __LOC__
    (J.decodeBoolean (J.string "test")) None;
  eq __LOC__
    (J.decodeBoolean (J.boolean true)) (Some true);
  eq __LOC__
    (J.decodeBoolean (J.array [||])) None;
  eq __LOC__
    (J.decodeBoolean J.null) None;
  eq __LOC__
    (J.decodeBoolean (J.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__
    (J.decodeBoolean (J.number 1.23)) None

let () =
  eq __LOC__
    (J.decodeNull (J.string "test")) None;
  eq __LOC__
    (J.decodeNull (J.boolean true)) None;
  eq __LOC__
    (J.decodeNull (J.array [||])) None;
  eq __LOC__
    (J.decodeNull J.null) (Some Js.null);
  eq __LOC__
    (J.decodeNull (J.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__
    (J.decodeNull (J.number 1.23)) None


let id (type t ) (obj : t) : t  = 
    match J.serialize obj with 
    | None -> assert false 
    | Some x -> J.deserializeExn x  

let idtest obj = 
  eq __LOC__ obj (id obj)      
let ( ) = 
  idtest None;
  idtest [None, None, None];
  idtest (Belt.List.makeBy 500 (fun i -> if i mod 2 = 0 then None else Some 1));
  idtest (Belt.Array.makeBy 500 (fun i -> if i mod 2 = 0 then None else Some 1))

let () = Mt.from_pair_suites __MODULE__ !suites
