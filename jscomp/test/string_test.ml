

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

let rev_split_by_char c s = 
  let rec loop i l = 
    try 
      let i' = String.index_from s i c  in 
      let s' = String.sub s i (i' - i)  in 
      loop (i'+1) (if s' = "" then l else s'::l)  
    with Not_found -> (String.sub s i (String.length s - i) ):: l 
  in 
  loop 0 []

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
      Eq ("\\\"\\\"", String.escaped {|""|}));
  "rev_split_by_char", (fun _ -> 
      Eq ([""; "bbbb"; "bbbb"], rev_split_by_char 'a' "bbbbabbbba"))
]
