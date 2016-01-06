

let ff x =
  let a = 
    match x with
    | "0"
    |"1"
    |"2" -> 3
    | "3" -> 4 
    | "4" -> 6
    | "7" -> 7 
    | _ -> 8
  in a + 3


let gg x =
  let a = 
    match x with
    | 0
    | 1 
    | 2 -> 3
    | 3 -> 4 
    | 4 -> 6
    | 8 -> 7 
    | _ -> 8
  in a + 3

open Mt

;; from_pair_suites __FILE__ [
"mutliple switch", (fun _ -> 
  Eq(9, ff "4"));
 "int switch", (fun _ -> 
   Eq(9, gg 4));
  "escape_normal", (fun _ -> Eq("haha", String.escaped "haha"));
  "escape_bytes", (fun _ -> Eq( Bytes.of_string "haha", Bytes.escaped (Bytes.of_string "haha")));
  (* FIXME it used char pattern match *)
  "escape_quote", (fun _ ->
    Eq ("\\\"\\\"", String.escaped {|""|}))

]
