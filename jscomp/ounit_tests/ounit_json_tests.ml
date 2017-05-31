
let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

open Ext_json_parse
let (|?)  m (key, cb) =
    m  |> Ext_json.test key cb 

exception Parse_error 
let suites = 
  __FILE__ 
  >:::
  [
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
