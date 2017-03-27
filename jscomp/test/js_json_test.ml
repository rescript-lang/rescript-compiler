let suites :  Mt.pair_suites ref  = ref []

let add_test = 
  let counter = ref 0 in
  fun loc test -> 
    incr counter; 
    let id = (loc ^ " id " ^ (string_of_int !counter)) in 
    suites := (id, test) :: ! suites

let eq loc x y = 
  add_test loc (fun _ -> Mt.Eq (x, y)) 

let throws loc f = 
  add_test loc (fun _ -> Mt.ThrowAny f)

let false_ loc =
  add_test loc (fun _ -> Mt.Ok false)

let true_ loc =
  add_test loc (fun _ -> Mt.Ok true)

let () =
  eq __LOC__
    (Js.Json.parse "null") (Ok (Obj.magic Js.null));
  eq __LOC__
    (Js.Json.parse "-") (Error "SyntaxError: Unexpected end of JSON input");
  throws __LOC__
    (fun () -> ignore (Js.Json.exnParse "-"))

let () = 
  let v = Js.Json.exnParse {| { "x" : [1, 2, 3 ] } |} in

  add_test __LOC__ (fun _ -> 
    let ty, x = Js.Json.reifyType v in
    match (ty : _ Js.Json.kind) with
    | Js.Json.Object ->  (* compiler infer x : Js.Json.t Js.Dict.t *) 
      begin match Js.Dict.get x "x" with 
      | Some v -> 
        let ty2, x = Js.Json.reifyType v in
        begin match ty2 with 
        | Js.Json.Array ->  (* compiler infer x : Js.Json.t array *)
          x 
          |> Js.Array.forEach (fun  x -> 
              let (ty3, x) = Js.Json.reifyType x in 
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
  let json = Js.Json.null |> Js.Json.stringify |> Js.Json.exnParse in 
  let ty, x = Js.Json.reifyType json in
  match ty with
  | Js.Json.Null -> true_ __LOC__
  | _ -> Js.log x; false_ __LOC__

let () = 
  let json = 
    Js.Json.string "test string" 
    |> Js.Json.stringify |> Js.Json.exnParse 
  in 
  let ty, x = Js.Json.reifyType json in
  match ty with
  | Js.Json.String -> eq __LOC__ x "test string"
  | _ -> false_ __LOC__

let () = 
  let json = 
    Js.Json.number 1.23456789
    |> Js.Json.stringify |> Js.Json.exnParse 
  in 
  let ty, x = Js.Json.reifyType json in
  match ty with
  | Js.Json.Number -> eq __LOC__ x 1.23456789
  | _ -> add_test __LOC__ (fun _ -> Mt.Ok false) 

let () = 
  let json = 
    Js.Json.number (float_of_int 0xAFAFAFAF)
    |> Js.Json.stringify |> Js.Json.exnParse 
  in 
  let ty, x = Js.Json.reifyType json in
  match ty with
  | Js.Json.Number -> eq __LOC__ (int_of_float x) 0xAFAFAFAF
  | _ -> add_test __LOC__ (fun _ -> Mt.Ok false) 

let () = 
  let test v = 
    let json = 
        Js.Json.boolean v |> Js.Json.stringify |> Js.Json.exnParse 
    in 
    let ty, x = Js.Json.reifyType json in
    match ty with
    | Js.Json.Boolean -> eq __LOC__ x v
    | _ -> false_ __LOC__
  in
  test Js.true_; 
  test Js.false_;
  ()
 
let option_get = function | None -> assert false | Some x -> x

let () = 
  let dict = Js_dict.empty  () in 
  Js_dict.set dict "a" (Js.Json.string "test string"); 
  Js_dict.set dict "b" (Js.Json.number 123.0); 

  let json = 
    dict |> Js.Json.object_ |> Js.Json.stringify |> Js.Json.exnParse 
  in

  (* Make sure parsed as Object *)
  let ty, x = Js.Json.reifyType json in
  match ty with
  | Js.Json.Object -> 

    (* Test field 'a' *)
    let ta, a = Js.Json.reifyType (option_get @@ Js_dict.get x "a") in 
    begin match ta with
    | Js.Json.String -> 
      if a <> "test string" 
      then false_ __LOC__
      else
        (* Test field 'b' *)
        let ty, b = Js.Json.reifyType (option_get @@ Js_dict.get x "b") in 
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
      (json:Js.Json.t) 
      (i:int) 
      (kind:a Js.Json.kind) 
      (expected:a) : unit = 

  let ty, x = Js.Json.reifyType json in 
  match ty with
  | Js.Json.Array -> 
    let ty, a1 = Js.Json.reifyType x.(i) in 
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
    |> Js.Json.stringify
    |> Js.Json.exnParse 
  in 
  eq_at_i __LOC__ json 0 Js.Json.String "string 0";
  eq_at_i __LOC__ json 1 Js.Json.String "string 1";
  eq_at_i __LOC__ json 2 Js.Json.String "string 2";
  ()

let () = 
  let json = 
    [| "string 0"; "string 1"; "string 2" |]
    |> Js.Json.stringArray
    |> Js.Json.stringify
    |> Js.Json.exnParse 
  in 
  eq_at_i __LOC__ json 0 Js.Json.String "string 0";
  eq_at_i __LOC__ json 1 Js.Json.String "string 1";
  eq_at_i __LOC__ json 2 Js.Json.String "string 2";
  ()

let () = 
  let a = [| 1.0000001; 10000000000.1; 123.0 |] in
  let json = 
    a  
    |> Js.Json.numberArray
    |> Js.Json.stringify
    |> Js.Json.exnParse 
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
    |> Array.map float_of_int
    |> Js.Json.numberArray
    |> Js.Json.stringify
    |> Js.Json.exnParse 
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
    |> Array.map Js_boolean.to_js_boolean
    |> Js.Json.booleanArray
    |> Js.Json.stringify
    |> Js.Json.exnParse 
  in 
  (* Loop is unrolled to keep relevant location information *)
  eq_at_i __LOC__ json 0 Js.Json.Boolean (Js_boolean.to_js_boolean a.(0));
  eq_at_i __LOC__ json 1 Js.Json.Boolean (Js_boolean.to_js_boolean a.(1));
  eq_at_i __LOC__ json 2 Js.Json.Boolean (Js_boolean.to_js_boolean a.(2));
  ()

let () =
  let make_d s i = 
    let d = Js_dict.empty() in 
    Js_dict.set d "a" (Js.Json.string s); 
    Js_dict.set d "b" (Js.Json.number (float_of_int i));
    d
  in 

  let a = [| make_d "aaa" 123; make_d "bbb" 456 |] in 
  let json = 
    a 
    |> Js.Json.objectArray
    |> Js.Json.stringify
    |> Js.Json.exnParse 
  in

  let ty, x = Js.Json.reifyType json in 
  match ty with
  | Js.Json.Array -> 
    let ty, a1 = Js.Json.reifyType x.(1) in 
    begin match ty with
    | Js.Json.Object-> 
      let ty, aValue =  Js.Json.reifyType @@ option_get @@ Js_dict.get a1 "a" in 
      begin match ty with
      | Js.Json.String -> eq __LOC__ aValue "bbb"
      | _ -> false_ __LOC__
      end
    | _ -> false_ __LOC__
    end
  | _ -> false_ __LOC__

let () = 
  let invalid_json_str = "{{ A}" in
  try
    let _ = Js.Json.exnParse invalid_json_str in
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

let () = eq __LOC__ (Js.Json.stringifyAny Js.null) (Some "null")

let () = eq __LOC__ (Js.Json.stringifyAny Js.Undefined.empty) None

let () = 
  eq __LOC__ 
    (Js.Json.decodeString (Js.Json.string "test")) (Some "test");
  eq __LOC__ 
    (Js.Json.decodeString (Js.Json.boolean Js.true_)) None;
  eq __LOC__ 
    (Js.Json.decodeString (Js.Json.array_ [||])) None;
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
    (Js.Json.decodeNumber (Js.Json.array_ [||])) None;
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
    (Js.Json.decodeObject (Js.Json.array_ [||])) None;
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
    (Js.Json.decodeArray (Js.Json.array_ [||])) (Some [||]);
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
    (Js.Json.decodeBoolean (Js.Json.array_ [||])) None;
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
    (Js.Json.decodeNull (Js.Json.array_ [||])) None;
  eq __LOC__ 
    (Js.Json.decodeNull Js.Json.null) (Some Js.null);
  eq __LOC__ 
    (Js.Json.decodeNull (Js.Json.object_ @@ Js.Dict.empty ())) None;
  eq __LOC__ 
    (Js.Json.decodeNull (Js.Json.number 1.23)) None





let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (boolean (Encode.boolean Js.true_)) (Ok Js.true_);
  eq __LOC__ 
    (boolean (Encode.float 1.23)) (Error "Expected boolean, got 1.23");
  eq __LOC__ 
    (boolean (Encode.int 23)) (Error "Expected boolean, got 23");
  eq __LOC__ 
    (boolean (Encode.string "test")) (Error "Expected boolean, got \"test\"");
  eq __LOC__ 
    (boolean Encode.null) (Error "Expected boolean, got null");
  eq __LOC__ 
    (boolean (Encode.array [||])) (Error "Expected boolean, got []");
  eq __LOC__ 
    (boolean (Encode.object_ @@ Js.Dict.empty ())) (Error "Expected boolean, got {}")

let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (float (Encode.boolean Js.true_)) (Error "Expected number, got true");
  eq __LOC__ 
    (float (Encode.float 1.23)) (Ok 1.23);
  eq __LOC__ 
    (float (Encode.int 23)) (Ok 23.);
  eq __LOC__ 
    (float (Encode.string "test")) (Error "Expected number, got \"test\"");
  eq __LOC__ 
    (float Encode.null) (Error "Expected number, got null");
  eq __LOC__ 
    (float (Encode.array [||])) (Error "Expected number, got []");
  eq __LOC__ 
    (float (Encode.object_ @@ Js.Dict.empty ())) (Error "Expected number, got {}")

let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (int (Encode.boolean Js.true_)) (Error "Expected number, got true");
  eq __LOC__ 
    (int (Encode.float 1.23)) (Error "Expected integer, got 1.23");
  eq __LOC__ 
    (int (Encode.int 23)) (Ok 23);
  eq __LOC__ 
    (int (Encode.string "test")) (Error "Expected number, got \"test\"");
  eq __LOC__ 
    (int Encode.null) (Error "Expected number, got null");
  eq __LOC__ 
    (int (Encode.array [||])) (Error "Expected number, got []");
  eq __LOC__ 
    (int (Encode.object_ @@ Js.Dict.empty ())) (Error "Expected number, got {}")

let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (string (Encode.boolean Js.true_)) (Error "Expected string, got true");
  eq __LOC__ 
    (string (Encode.float 1.23)) (Error "Expected string, got 1.23");
  eq __LOC__ 
    (string (Encode.int 23)) (Error "Expected string, got 23");
  eq __LOC__ 
    (string (Encode.string "test")) (Ok "test");
  eq __LOC__ 
    (string Encode.null) (Error "Expected string, got null");
  eq __LOC__ 
    (string (Encode.array [||])) (Error "Expected string, got []");
  eq __LOC__ 
    (string (Encode.object_ @@ Js.Dict.empty ())) (Error "Expected string, got {}")

let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (nullable int (Encode.boolean Js.true_)) (Error "Expected number, got true");
  eq __LOC__ 
    (nullable int (Encode.float 1.23)) (Error "Expected integer, got 1.23");
  eq __LOC__ 
    (nullable int (Encode.int 23)) (Ok (Js.Null.return 23));
  eq __LOC__ 
    (nullable int (Encode.string "test")) (Error "Expected number, got \"test\"");
  eq __LOC__ 
    (nullable int Encode.null) (Ok Js.null);
  eq __LOC__ 
    (nullable int (Encode.array [||])) (Error "Expected number, got []");
  eq __LOC__ 
    (nullable int (Encode.object_ @@ Js.Dict.empty ())) (Error "Expected number, got {}");
  eq __LOC__ 
    (nullable boolean (Encode.boolean Js.true_)) (Ok (Js.Null.return Js.true_));
  eq __LOC__ 
    (nullable float (Encode.float 1.23)) (Ok (Js.Null.return 1.23));
  eq __LOC__ 
    (nullable string (Encode.string "test")) (Ok (Js.Null.return "test"));
  eq __LOC__ 
    (nullable (nullAs Js.null) Encode.null) (Ok Js.null);
  eq __LOC__ 
    (nullable boolean (Encode.int 1)) (Error "Expected boolean, got 1")

let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (nullAs 0 (Encode.boolean Js.true_)) (Error "Expected null, got true");
  eq __LOC__ 
    (nullAs 0 (Encode.float 1.23)) (Error "Expected null, got 1.23");
  eq __LOC__ 
    (nullAs 0 (Encode.int 23)) (Error "Expected null, got 23");
  eq __LOC__ 
    (nullAs 0 (Encode.string "test")) (Error "Expected null, got \"test\"");
  eq __LOC__ 
    (nullAs 0 Encode.null) (Ok 0);
  eq __LOC__ 
    (nullAs 0 (Encode.array [||])) (Error "Expected null, got []");
  eq __LOC__ 
    (nullAs 0 (Encode.object_ @@ Js.Dict.empty ())) (Error "Expected null, got {}");
  eq __LOC__ 
    (nullAs Js.null Encode.null) (Ok Js.null);
  eq __LOC__ 
    (nullAs None Encode.null) (Ok None);
  eq __LOC__ 
    (nullAs (Some "foo") Encode.null) (Ok (Some "foo"))

let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (array int (Encode.boolean Js.true_)) (Error "Expected array, got true");
  eq __LOC__ 
    (array int (Encode.float 1.23)) (Error "Expected array, got 1.23");
  eq __LOC__ 
    (array int (Encode.int 23)) (Error "Expected array, got 23");
  eq __LOC__ 
    (array int (Encode.string "test")) (Error "Expected array, got \"test\"");
  eq __LOC__ 
    (array int Encode.null) (Error "Expected array, got null");
  eq __LOC__ 
    (array int (Encode.array [||])) (Ok [||]);
  eq __LOC__ 
    (array int (Encode.object_ @@ Js.Dict.empty ())) (Error "Expected array, got {}");
  eq __LOC__ 
    (array boolean (exnParse {| [true, false, true] |})) (Ok [| Js.true_; Js.false_; Js.true_ |]);
  eq __LOC__ 
    (array float (exnParse {| [1, 2, 3] |})) (Ok [| 1.; 2.; 3. |]);
  eq __LOC__ 
    (array int (exnParse {| [1, 2, 3] |})) (Ok [| 1; 2; 3 |]);
  eq __LOC__ 
    (array string (exnParse {| ["a", "b", "c"] |})) (Ok [| "a"; "b"; "c" |]);
  eq __LOC__ 
    (array (nullAs Js.null) (exnParse {| [null, null, null] |})) (Ok [| Js.null; Js.null; Js.null |]);
  eq __LOC__ 
    (array boolean (exnParse {| [1, 2, 3] |})) (Error "Expected boolean, got 1")

let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (dict int (Encode.boolean Js.true_)) (Error "Expected object, got true");
  eq __LOC__ 
    (dict int (Encode.float 1.23)) (Error "Expected object, got 1.23");
  eq __LOC__ 
    (dict int (Encode.int 23)) (Error "Expected object, got 23");
  eq __LOC__ 
    (dict int (Encode.string "test")) (Error "Expected object, got \"test\"");
  eq __LOC__ 
    (dict int Encode.null) (Error "Expected object, got null");
  eq __LOC__ 
    (dict int (Encode.array [||])) (Error "Expected object, got []");
  eq __LOC__ 
    (dict int (Encode.object_ @@ Js.Dict.empty ())) 
    (Ok (Js.Dict.empty ()));
  eq __LOC__ 
    (dict boolean (exnParse {| { "a": true, "b": false } |}))
    (Ok (Obj.magic [%obj { a = true; b = false }]));
  eq __LOC__ 
    (dict float (exnParse {| { "a": 1.2, "b": 2.3 } |}))
    (Ok (Obj.magic [%obj { a = 1.2; b = 2.3 }]));
  eq __LOC__ 
    (dict int (exnParse {| { "a": 1, "b": 2 } |}))
    (Ok (Obj.magic [%obj { a = 1; b = 2 }]));
  eq __LOC__ 
    (dict string (exnParse {| { "a": "x", "b": "y" } |}))
    (Ok (Obj.magic [%obj { a = "x"; b = "y" }]));
  eq __LOC__ 
    (dict (nullAs Js.null) (exnParse {| { "a": null, "b": null } |}))
    (Ok (Obj.magic [%obj { a = Js.null; b = Js.null }]));
  eq __LOC__ 
    (dict string (exnParse {| { "a": null, "b": null } |}))
    (Error "Expected string, got null")

let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (field "foo" int (Encode.boolean Js.true_)) (Error "Expected object, got true");
  eq __LOC__ 
    (field "foo" int (Encode.float 1.23)) (Error "Expected object, got 1.23");
  eq __LOC__ 
    (field "foo" int (Encode.int 23)) (Error "Expected object, got 23");
  eq __LOC__ 
    (field "foo" int (Encode.string "test")) (Error "Expected object, got \"test\"");
  eq __LOC__ 
    (field "foo" int Encode.null) (Error "Expected object, got null");
  eq __LOC__ 
    (field "foo" int (Encode.array [||])) (Error "Expected object, got []");
  eq __LOC__ 
    (field "foo" int (Encode.object_ @@ Js.Dict.empty ())) 
    (Error "Expected field 'foo'");
  eq __LOC__ 
    (field "b" boolean (exnParse {| { "a": true, "b": false } |}))
    (Ok Js.false_);
  eq __LOC__ 
    (field "b" float (exnParse {| { "a": 1.2, "b": 2.3 } |}))
    (Ok 2.3);
  eq __LOC__ 
    (field "b" int (exnParse {| { "a": 1, "b": 2 } |}))
    (Ok 2);
  eq __LOC__ 
    (field "b" string (exnParse {| { "a": "x", "b": "y" } |}))
    (Ok "y");
  eq __LOC__ 
    (field "b" (nullAs Js.null) (exnParse {| { "a": null, "b": null } |}))
    (Ok Js.null);
  eq __LOC__ 
    (field "b" string (exnParse {| { "a": null, "b": null } |}))
    (Error "Expected string, got null")

let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (optional int (Encode.boolean Js.true_)) (Ok None);
  eq __LOC__ 
    (optional int (Encode.float 1.23)) (Ok None);
  eq __LOC__ 
    (optional int (Encode.int 23)) (Ok (Some 23));
  eq __LOC__ 
    (optional int (Encode.string "test")) (Ok None);
  eq __LOC__ 
    (optional int Encode.null) (Ok None);
  eq __LOC__ 
    (optional int (Encode.array [||])) (Ok None);
  eq __LOC__ 
    (optional int (Encode.object_ @@ Js.Dict.empty ())) (Ok None);
  eq __LOC__ 
    (optional boolean (Encode.boolean Js.true_)) (Ok (Some Js.true_));
  eq __LOC__ 
    (optional float (Encode.float 1.23)) (Ok (Some 1.23));
  eq __LOC__ 
    (optional string (Encode.string "test")) (Ok (Some "test"));
  eq __LOC__ 
    (optional (nullAs Js.null) Encode.null) (Ok (Some Js.null));
  eq __LOC__ 
    (optional boolean (Encode.int 1)) (Ok None);
  eq __LOC__ 
    (optional (field "x" int) (exnParse {| { "x": 2} |})) (Ok (Some 2));
  eq __LOC__ 
    (optional (field "x" int) (exnParse {| { "x": 2.3} |})) (Ok None);
  eq __LOC__ 
    (optional (field "y" int) (exnParse {| { "x": 2} |})) (Ok None);
  eq __LOC__ 
    (field "x" (optional int) (exnParse {| { "x": 2} |})) (Ok (Some 2));
  eq __LOC__ 
    (field "x" (optional int) (exnParse {| { "x": 2.3} |})) (Ok None);
  eq __LOC__ 
    (field "y" (optional int) (exnParse {| { "x": 2} |})) (Error "Expected field 'y'")

(* composite decode *)
let () = 
  let open Js.Json in
  let open Decode in
  eq __LOC__ 
    (dict (array (array int)) (exnParse {| { "a": [[1, 2], [3]], "b": [[4], [5, 6]] } |}))
    (Ok (Obj.magic [%obj { a = [| [|1; 2|]; [|3|] |]; b = [| [|4|]; [|5; 6|] |] }]));
  eq __LOC__ 
    (dict (array (array int)) (exnParse {| { "a": [[1, 2], [true]], "b": [[4], [5, 6]] } |}))
    (Error "Expected number, got true");
  eq __LOC__ 
    (dict (array (array int)) (exnParse {| { "a": [[1, 2], "foo"], "b": [[4], [5, 6]] } |}))
    (Error "Expected array, got \"foo\"");
  eq __LOC__ 
    (let json = exnParse {| { "foo": [1, 2, 3], "bar": "baz" } |} in
    (field "foo" (array int) json, field "bar" string json))
    (Ok [| 1; 2; 3 |], Ok "baz")

let () = Mt.from_pair_suites __FILE__ !suites