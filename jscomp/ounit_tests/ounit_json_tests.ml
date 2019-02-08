
let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

open Ext_json_parse
let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb 

let rec strip (x : Ext_json_types.t) : Ext_json_noloc.t = 
  let open Ext_json_noloc in 
  match x with 
  | True _ -> true_
  | False _ -> false_
  | Null _ -> null
  | Flo {flo = s} -> flo s 
  | Str {str = s} -> str s 
  | Arr {content } -> arr (Array.map strip content)
  | Obj {map} -> 
    obj (String_map.map map strip)

let id_parsing_serializing x = 
  let normal_s = 
    Ext_json_noloc.to_string 
      @@ strip 
      @@ Ext_json_parse.parse_json_from_string x  
  in 
  let normal_ss = 
    Ext_json_noloc.to_string 
    @@ strip 
    @@ Ext_json_parse.parse_json_from_string normal_s
  in 
  if normal_s <> normal_ss then 
    begin 
      prerr_endline "ERROR";
      prerr_endline normal_s ;
      prerr_endline normal_ss ;
    end;
  OUnit.assert_equal ~cmp:(fun (x:string) y -> x = y) normal_s normal_ss

let id_parsing_x2 x = 
  let stru = Ext_json_parse.parse_json_from_string x |> strip in 
  let normal_s = Ext_json_noloc.to_string stru in 
  let normal_ss = strip (Ext_json_parse.parse_json_from_string normal_s) in 
  if Ext_json_noloc.equal stru normal_ss then 
    true
  else begin 
    prerr_endline "ERROR";
    prerr_endline normal_s;
    Format.fprintf Format.err_formatter 
    "%a@.%a@." Ext_obj.pp_any stru Ext_obj.pp_any normal_ss; 
    
    prerr_endline (Ext_json_noloc.to_string normal_ss);
    false
  end  

let test_data = 
  [{|
      {}
      |};
   {| [] |};
   {| [1,2,3]|};
   {| ["x", "y", 1,2,3 ]|};
   {| { "x" :  3, "y" : "x", "z" : [1,2,3, "x"] }|};
   {| {"x " : true , "y" : false , "z\"" : 1} |}
  ] 
exception Parse_error 
let suites = 
  __FILE__ 
  >:::
  [

    __LOC__ >:: begin fun _ -> 
      List.iter id_parsing_serializing test_data
    end;

    __LOC__ >:: begin fun _ -> 
      List.iteri (fun i x -> OUnit.assert_bool (__LOC__ ^ string_of_int i ) (id_parsing_x2 x)) test_data
    end;
    "empty_json" >:: begin fun _ -> 
      let v =parse_json_from_string "{}" in
      match v with 
      | Obj {map = v} -> OUnit.assert_equal (String_map.is_empty v ) true
      | _ -> OUnit.assert_failure "should be empty"
    end
    ;
    "empty_arr" >:: begin fun _ -> 
      let v =parse_json_from_string "[]" in
      match v with 
      | Arr {content = [||]} -> ()
      | _ -> OUnit.assert_failure "should be empty"
    end
    ;
    "empty trails" >:: begin fun _ -> 
      (OUnit.assert_raises Parse_error @@ fun _ -> 
       try parse_json_from_string {| [,]|} with _ -> raise Parse_error);
      OUnit.assert_raises Parse_error @@ fun _ -> 
      try parse_json_from_string {| {,}|} with _ -> raise Parse_error
    end;
    "two trails" >:: begin fun _ -> 
      (OUnit.assert_raises Parse_error @@ fun _ -> 
       try parse_json_from_string {| [1,2,,]|} with _ -> raise Parse_error);
      (OUnit.assert_raises Parse_error @@ fun _ -> 
       try parse_json_from_string {| { "x": 3, ,}|} with _ -> raise Parse_error)
    end;

    "two trails fail" >:: begin fun _ -> 
      (OUnit.assert_raises Parse_error @@ fun _ -> 
       try parse_json_from_string {| { "x": 3, 2 ,}|} with _ -> raise Parse_error)
    end;

    "trail comma obj" >:: begin fun _ -> 
      let v =  parse_json_from_string {| { "x" : 3 , }|} in 
      let v1 =  parse_json_from_string {| { "x" : 3 , }|} in 
      let test (v : Ext_json_types.t)  = 
        match v with 
        | Obj {map = v} -> 
          v
          |? ("x" , `Flo (fun x -> OUnit.assert_equal x "3"))
          |> ignore 
        | _ -> OUnit.assert_failure "trail comma" in 
      test v ;
      test v1 
    end
    ;
    "trail comma arr" >:: begin fun _ -> 
      let v = parse_json_from_string {| [ 1, 3, ]|} in
      let v1 = parse_json_from_string {| [ 1, 3 ]|} in
      let test (v : Ext_json_types.t) = 
        match v with 
        | Arr { content = [| Flo {flo = "1"} ; Flo { flo = "3"} |] } -> ()
        | _ -> OUnit.assert_failure "trailing comma array" in 
      test v ;
      test v1
    end
  ]
