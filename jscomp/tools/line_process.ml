
(* let lexer = Genlex.make_lexer [] (\* poor man *\) *)

(* let rec to_list acc stream =  *)
(*   match Stream.next stream with  *)
(*   | exception _ -> List.rev acc  *)
(*   | v -> to_list (v::acc) stream  *)
 

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


(** on 32 bit , there are 16M limitation *)
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let (@>) (b, v) acc = 
  if b then 
    v :: acc 
  else
      acc 


let (//) = Filename.concat

let rec process_line cwd filedir  line = 
  let line = Ext_string.trim line in 
  let len = String.length line in 
  if len = 0 then []
  else 
    match line.[0] with 
    | '#' -> []
    | _ -> 
      let segments = 
        Ext_string.split_by ~keep_empty:false (fun x -> x = ' ' || x = '\t' ) line 
      in

      begin 
        match segments with 
        | ["include" ;  path ]
          ->  
          (* prerr_endline path;  *)
          read_lines cwd  (filedir// path)
        | [ x ]  -> 
          let ml = filedir // x ^ ".ml" in 
          let mli = filedir // x ^ ".mli" in 
          let ml_exists, mli_exists = Sys.file_exists ml , Sys.file_exists mli in 
          if not ml_exists && not mli_exists then 
            begin 
              prerr_endline (filedir //x ^ " not exists"); 
              []
            end
          else 
            (ml_exists, ml) @> (mli_exists , mli) @> []            

        | _ 
          ->  Ext_pervasives.failwithf "invalid line %s" line
      end

(* example 
   {[ 
     Line_process.read_lines "." "./tools/tools.mllib" 
   ]} 

   FIXME: we can only concat (dir/file) not (dir/dir)
   {[
     Filename.concat "/bb/x/" "/bb/x/";;
   ]}
*)
and read_lines cwd file = 

  file 
  |> rev_lines_of_file 
  |> List.fold_left (fun acc f ->
      let filedir  =   Filename.dirname file in
      let extras = process_line  cwd filedir f in 
      extras  @ acc       
    ) []
