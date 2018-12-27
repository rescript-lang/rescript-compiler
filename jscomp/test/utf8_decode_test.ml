

type byte = 
  | Single of int
  | Cont of int
  | Leading of int * int
  | Invalid
 
(** [classify chr] returns the {!byte} corresponding to [chr] *)
let classify chr =
    let c = int_of_char chr in
    (* Classify byte according to leftmost 0 bit *)
    if c land 0b1000_0000 = 0 then Single c else 
      (* c 0b0____*)
    if c land 0b0100_0000 = 0 then Cont (c land 0b0011_1111) else
      (* c 0b10___*)
    if c land 0b0010_0000 = 0 then Leading (1, c land 0b0001_1111) else
      (* c 0b110__*)
    if c land 0b0001_0000 = 0 then Leading (2, c land 0b0000_1111) else
      (* c 0b1110_ *)
    if c land 0b0000_1000 = 0 then Leading (3, c land 0b0000_0111) else
      (* c 0b1111_0___*)
    if c land 0b0000_0100 = 0 then Leading (4, c land 0b0000_0011) else
      (* c 0b1111_10__*)
    if c land 0b0000_0010 = 0 then Leading (5, c land 0b0000_0001)
       (* c 0b1111_110__ *)
    else Invalid
 
 
(** [utf8_decode strm] returns a code point stream that will lazily decode
    the byte stream [strm] *)
let rec utf8_decode strm =
    Stream.slazy (fun () ->
        match Stream.peek strm with
        | Some chr ->
            Stream.junk strm;
            (match classify chr with
            | Single c -> Stream.icons c (utf8_decode strm)
            | Cont _ -> raise (Stream.Error "Unexpected continuation byte")
            | Leading (n, c) ->
              (** [follow strm n c] returns the code point based on [c] plus [n] continuation
                  bytes taken from [strm] *)
              let rec follow strm n c =
                if n = 0 then c
                else
                  (match classify (Stream.next strm) with
                   | Cont cc -> follow strm (n-1) ((c lsl 6) lor (cc land 0x3f))
                   | _ -> raise (Stream.Error "Continuation byte expected"))
              in
              Stream.icons (follow strm n c)  (utf8_decode strm)
            | Invalid -> raise (Stream.Error "Invalid byte"))
        | None -> Stream.sempty)
let to_list xs =
  let v = ref [] in 
  Stream.iter  (fun x -> v := x :: !v) xs ;
  List.rev !v 

let utf8_list (s : string) : int list =   
  to_list (utf8_decode (Stream.of_string s ))


let  decode bytes offset  =
  let rec  init offset = 
    match classify (Bytes.get bytes offset) with 
    | Single c ->  c, offset + 1 
    | Invalid 
    | Cont _ -> invalid_arg "decode" 
    | Leading(n,c) -> leading n c (offset  + 1)
  and leading n c offset  = 
    if n = 0 then c, offset 
    else
      begin match classify (Bytes.get bytes offset) with 
        | Cont cc -> leading (n - 1) ((c lsl 6) lor (cc land 0x3f)) (offset + 1 )
        | _ -> invalid_arg "decode"
      end 
  in init offset



let rec  eq_list cmp xs ys =
  match xs,ys with 
  | [], [] ->  true 
  | _, [] -> false
  | [], _ -> false 
  | x::xs, y::ys -> cmp x y && eq_list cmp xs ys 

let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) = 
  incr test_id ; 
  Js.log (x,y);
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let () = 
  let tbl = 
    [
      "你好BuckleScript,最好的JS语言",
      [20320; 22909; 66; 117; 99; 107; 108; 101; 83; 99; 114; 105; 112; 116; 44;
       26368; 22909; 30340; 74; 83; 35821; 35328] ; 

      "hello 你好，中华民族 hei" ,
      [104; 101; 108; 108; 111; 32; 20320; 22909; 65292; 20013; 21326; 27665; 26063; 32; 104; 101; 105]

    ] in 
  List.iter (fun (s, code) -> 
    eq __LOC__ (true, eq_list ((=) : int -> _ -> _ ) (utf8_list s) code )
  )
  tbl
  

let () = 
  Mt.from_pair_suites __MODULE__ !suites
(**
uutf:
let test vv = 
   let  v = ref [] in 
   let add u = v := u :: !v  in
   (try  while true do
   add (Uutf.decode vv |> (function `Uchar x -> x | _ -> assert false ))
   done  with _ -> ());
   List.rev !v
;;
let codes = test (Uutf.decoder (`String "你好BuckleScript,最好的JS语言") )
*)
