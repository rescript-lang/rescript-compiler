
let lexer = Genlex.make_lexer [] (* poor man *)

let rec to_list acc stream = 
  match Stream.next stream with 
  | exception _ -> List.rev acc 
  | v -> to_list (v::acc) stream 
 
let process_line line = 
  match to_list [] (lexer (Stream.of_string line)) with
  | Ident "#" :: _ -> None
  | (Ident v|Kwd v) :: _ -> Some v 
  | (Int _ | Float _ | Char _ | String _ )  :: _ -> 
      assert false 
  | [] -> None 

let rev_lines_of_file file = 
  let chan = open_in file in
  let rec loop acc = 
    match input_line chan with
    | line -> loop (line :: acc)
    | exception End_of_file -> close_in chan ; acc in
  loop []


let rec filter_map (f: 'a -> 'b option) xs = 
  match xs with 
  | [] -> []
  | y :: ys -> 
      begin match f y with 
      | None -> filter_map f ys
      | Some z -> z :: filter_map f ys
      end

let trim s = 
  let i = ref 0  in
  let j = String.length s in 
  while !i < j &&  let u = s.[!i] in u = '\t' || u = '\n' || u = ' ' do 
    incr i;
  done;
  let k = ref (j - 1)  in 
  while !k >= !i && let u = s.[!k] in u = '\t' || u = '\n' || u = ' ' do 
    decr k ;
  done;
  String.sub s !i (!k - !i + 1)



let (@>) v acc = 
  if Sys.file_exists v then 
    v :: acc 
  else acc 

let read_lines file = 
  file 
  |> rev_lines_of_file 
  |> List.fold_left (fun acc f -> 
      match process_line f with 
      | None -> acc 
      | Some f -> 
         (f ^ ".mli") @> (f ^ ".ml") @> acc
    ) []
