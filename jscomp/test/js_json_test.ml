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

  let v = Js.Json.parseExn {| { "x" : [1, 2, 3 ] } |} in

  add_test __LOC__ (fun _ -> 
    match Js.Json.classify v with
    | JSONObject x ->  (* compiler infer x : Js.Json.t Js.Dict.t *) 
      begin match Js.Dict.get x "x" with 
      | Some v -> 
        begin match Js.Json.classify v with 
        | JSONArray x ->  (* compiler infer x : Js.Json.t array *)
          x |> Js.Array.forEach (fun  x -> 
              match Js.Json.classify x with 
              | JSONNumber _ -> () 
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
  let json = Js.Json.null |> Js.Json.stringify |> Js.Json.parseExn in 
  match Js.Json.classify json with
  | JSONNull -> true_ __LOC__
  | x -> Js.log x; false_ __LOC__

let () = 
  let json = 
    Js.Json.string "test string" 
    |> Js.Json.stringify |> Js.Json.parseExn 
  in 
  match Js.Json.classify json with
  | JSONString x -> eq __LOC__ x "test string"
  | _ -> false_ __LOC__

let () = 
  let json = 
    Js.Json.number 1.23456789
    |> Js.Json.stringify |> Js.Json.parseExn 
  in 
  match Js.Json.classify json with
  | JSONNumber x -> eq __LOC__ x 1.23456789
  | _ -> add_test __LOC__ (fun _ -> Mt.Ok false) 

let () = 
  let json = 
    Js.Json.number (float_of_int 0xAFAFAFAF)
    |> Js.Json.stringify |> Js.Json.parseExn 
  in 
  match Js.Json.classify json with
  | JSONNumber x -> eq __LOC__ (int_of_float x) 0xAFAFAFAF
  | _ -> add_test __LOC__ (fun _ -> Mt.Ok false) 

let () = 
  let test v = 
    let json = 
        Js.Json.boolean v |> Js.Json.stringify |> Js.Json.parseExn 
    in 
    match Js.Json.classify json with
    | JSONTrue when v = Js.true_ -> true_ __LOC__
    | JSONFalse when v = Js.false_ -> true_ __LOC__
    | _ -> false_ __LOC__
  in
  test Js.true_; 
  test Js.false_;
  ()
 
let option_get = function | None -> assert false | Some x -> x

let () = 
  let dict = Js_dict.empty  () in 
  Js_dict.set dict "a" (Js_json.string "test string"); 
  Js_dict.set dict "b" (Js_json.number 123.0); 

  let json = 
    dict |> Js.Json.object_ |> Js.Json.stringify |> Js.Json.parseExn 
  in

  (* Make sure parsed as Object *)
  match Js.Json.classify json with
  | JSONObject x -> 

    (* Test field 'a' *)
    begin match Js.Json.classify (option_get @@ Js_dict.get x "a") with
    | JSONString a -> 
      if a <> "test string" 
      then false_ __LOC__
      else
        (* Test field 'b' *)
        begin match Js.Json.classify (option_get @@ Js_dict.get x "b") with
        | JSONNumber b -> 
          add_test __LOC__ (fun _ -> Mt.Approx (123.0, b))
        | _ -> false_ __LOC__
        end 
    | _ -> false_ __LOC__
    end
  | _ -> false_ __LOC__

(* Check that the given json value is an array and that its element 
 * a position [i] is equal to both the [kind] and [expected] value *)
let eq_at_i 
      (loc:string)
      (json:Js_json.t) 
      (i:int) 
      (expected:Js.Json.tagged_t) : unit = 

  match Js.Json.classify json with
  | JSONArray x -> 
    eq loc (Js.Json.classify x.(i)) expected
  | _ -> false_ loc

let () = 
  let json = 
    [| "string 0"; "string 1"; "string 2" |]
    |> Array.map Js.Json.string
    |> Js.Json.array
    |> Js.Json.stringify
    |> Js.Json.parseExn 
  in 
  eq_at_i __LOC__ json 0 (JSONString "string 0");
  eq_at_i __LOC__ json 1 (JSONString "string 1");
  eq_at_i __LOC__ json 2 (JSONString "string 2");
  ()

let () = 
  let json = 
    [| "string 0"; "string 1"; "string 2" |]
    |> Js.Json.stringArray
    |> Js.Json.stringify
    |> Js.Json.parseExn 
  in 
  eq_at_i __LOC__ json 0 (JSONString "string 0");
  eq_at_i __LOC__ json 1 (JSONString "string 1");
  eq_at_i __LOC__ json 2 (JSONString "string 2");
  ()

let () = 
  let a = [| 1.0000001; 10000000000.1; 123.0 |] in
  let json = 
    a  
    |> Js.Json.numberArray
    |> Js.Json.stringify
    |> Js.Json.parseExn 
  in 
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 (JSONNumber a.(0));
  eq_at_i __LOC__ json 1 (JSONNumber a.(1));
  eq_at_i __LOC__ json 2 (JSONNumber a.(2));
  ()

let () = 
  let a = [| 0; 0xAFAFAFAF; 0xF000AABB|] in
  let json = 
    a  
    |> Array.map float_of_int
    |> Js.Json.numberArray
    |> Js.Json.stringify
    |> Js.Json.parseExn 
  in 
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 (JSONNumber (float_of_int a.(0)));
  eq_at_i __LOC__ json 1 (JSONNumber (float_of_int a.(1)));
  eq_at_i __LOC__ json 2 (JSONNumber (float_of_int a.(2)));
  ()

let () = 
  let a = [| true; false; true |] in
  let json = 
    a  
    |> Array.map Js_boolean.to_js_boolean
    |> Js.Json.booleanArray
    |> Js.Json.stringify
    |> Js.Json.parseExn 
  in
  let a' = Js.Array.map (fun x -> if x then Js.Json.JSONTrue else Js.Json.JSONFalse) a in
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 a'.(0);
  eq_at_i __LOC__ json 1 a'.(1);
  eq_at_i __LOC__ json 2 a'.(2);
  ()

let () =
  let make_d s i = 
    let d = Js_dict.empty() in 
    Js_dict.set d "a" (Js_json.string s); 
    Js_dict.set d "b" (Js_json.number (float_of_int i));
    d
  in 

  let a = [| make_d "aaa" 123; make_d "bbb" 456 |] in 
  let json = 
    a 
    |> Js.Json.objectArray
    |> Js.Json.stringify
    |> Js.Json.parseExn 
  in

  match Js.Json.classify json with
  | JSONArray x -> 
    begin match Js.Json.classify x.(1) with
    | JSONObject a1 -> 
      begin match Js.Json.classify @@ option_get @@ Js_dict.get a1 "a" with
      | JSONString aValue -> eq __LOC__ aValue "bbb"
      | _ -> false_ __LOC__
      end
    | _ -> false_ __LOC__
    end
  | _ -> false_ __LOC__

let () = 
  let invalid_json_str = "{{ A}" in
  try
    let _ = Js_json.parseExn invalid_json_str in
    false_ __LOC__
  with
  | exn -> 
    true_ __LOC__

(* stringifyAny tests *)

let () = eq __LOC__ (Js.Json.stringifyAny [|1; 2; 3|]) (Some "[1,2,3]")

let () =
  eq
  __LOC__
  (Js.Json.stringifyAny [%bs.obj {foo = 1; bar = "hello"; baz = [%bs.obj {baaz = 10}]}])
  (Some {|{"foo":1,"bar":"hello","baz":{"baaz":10}}|})

let () = eq __LOC__ (Js.Json.stringifyAny Js.Null.empty) (Some "null")

let () = eq __LOC__ (Js.Json.stringifyAny Js.Undefined.empty) None

let () = 
  eq __LOC__ 
    (Js.Json.decodeString (Js.Json.string "test")) (Some "test");
  eq __LOC__ 
    (Js.Json.decodeString (Js.Json.boolean Js.true_)) None;
  eq __LOC__ 
    (Js.Json.decodeString (Js.Json.array [||])) None;
  eq __LOC__ 
    (Js.Json.decodeString Js.Json.null) None;
  eq __LOC__ 
    (Js.Json.decodeString (Js.Json.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__ 
    (Js.Json.decodeString (Js.Json.number 1.23)) None

let () = 
  eq __LOC__ 
    (Js.Json.decodeNumber (Js.Json.string "test")) None;
  eq __LOC__ 
    (Js.Json.decodeNumber (Js.Json.boolean Js.true_)) None;
  eq __LOC__ 
    (Js.Json.decodeNumber (Js.Json.array [||])) None;
  eq __LOC__ 
    (Js.Json.decodeNumber Js.Json.null) None;
  eq __LOC__ 
    (Js.Json.decodeNumber (Js.Json.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__ 
    (Js.Json.decodeNumber (Js.Json.number 1.23)) (Some 1.23)

let () = 
  eq __LOC__ 
    (Js.Json.decodeObject (Js.Json.string "test")) None;
  eq __LOC__ 
    (Js.Json.decodeObject (Js.Json.boolean Js.true_)) None;
  eq __LOC__ 
    (Js.Json.decodeObject (Js.Json.array [||])) None;
  eq __LOC__ 
    (Js.Json.decodeObject Js.Json.null) None;
  eq __LOC__ 
    (Js.Json.decodeObject (Js.Json.object_ @@ Js.Dict.empty ())) 
    (Some (Js.Dict.empty ()));
  eq __LOC__ 
    (Js.Json.decodeObject (Js.Json.number 1.23)) None

let () = 
  eq __LOC__ 
    (Js.Json.decodeArray (Js.Json.string "test")) None;
  eq __LOC__ 
    (Js.Json.decodeArray (Js.Json.boolean Js.true_)) None;
  eq __LOC__ 
    (Js.Json.decodeArray (Js.Json.array [||])) (Some [||]);
  eq __LOC__ 
    (Js.Json.decodeArray Js.Json.null) None;
  eq __LOC__ 
    (Js.Json.decodeArray (Js.Json.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__ 
    (Js.Json.decodeArray (Js.Json.number 1.23)) None

let () = 
  eq __LOC__ 
    (Js.Json.decodeBoolean (Js.Json.string "test")) None;
  eq __LOC__ 
    (Js.Json.decodeBoolean (Js.Json.boolean Js.true_)) (Some Js.true_);
  eq __LOC__ 
    (Js.Json.decodeBoolean (Js.Json.array [||])) None;
  eq __LOC__ 
    (Js.Json.decodeBoolean Js.Json.null) None;
  eq __LOC__ 
    (Js.Json.decodeBoolean (Js.Json.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__ 
    (Js.Json.decodeBoolean (Js.Json.number 1.23)) None

let () = 
  eq __LOC__ 
    (Js.Json.decodeNull (Js.Json.string "test")) None;
  eq __LOC__ 
    (Js.Json.decodeNull (Js.Json.boolean Js.true_)) None;
  eq __LOC__ 
    (Js.Json.decodeNull (Js.Json.array [||])) None;
  eq __LOC__ 
    (Js.Json.decodeNull Js.Json.null) (Some Js.null);
  eq __LOC__ 
    (Js.Json.decodeNull (Js.Json.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__ 
    (Js.Json.decodeNull (Js.Json.number 1.23)) None

let () = Mt.from_pair_suites __FILE__ !suites
