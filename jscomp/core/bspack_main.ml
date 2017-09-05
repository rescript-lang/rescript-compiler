(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)




let (@>) (b, v) acc =
  if b then
    v :: acc
  else
    acc

let preprocess_string fn str oc =

  let lexbuf = Lexing.from_string  str in
  Lexer.init () ;
  Location.init lexbuf fn;
  let segments =
    lexbuf
    |> Lexer.filter_directive_from_lexbuf   in
  segments
  |> List.iter
    (fun (start, pos) ->
       output_substring  oc str start (pos - start)
    )

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
          ->  Ext_pervasives.failwithf ~loc:__LOC__ "invalid line %s" line
      end
and read_lines (cwd : string) (file : string) : string list =
  file
  |> Ext_io.rev_lines_of_file
  |> List.fold_left (fun acc f ->
      let filedir  =   Filename.dirname file in
      let extras = process_line  cwd filedir f in
      extras  @ acc
    ) []

let implementation sourcefile =
  let content = Ext_io.load_file sourcefile in
  let ast =
    let oldname = !Location.input_name in
    Location.input_name := sourcefile ;
    let lexbuf = Lexing.from_string content in
    Location.init lexbuf sourcefile ;
    match Parse.implementation  lexbuf
    with
    | exception e ->
      Location.input_name := oldname;
      raise e
    | ast ->
      Location.input_name := oldname ;
      ast
  in
  ast, content

let interface sourcefile =
  let content = Ext_io.load_file sourcefile in
  let ast =
    let oldname = !Location.input_name in
    Location.input_name := sourcefile ;
    let lexbuf = Lexing.from_string content in
    Location.init lexbuf sourcefile;
    match Parse.interface  lexbuf with
    | exception e ->
      Location.input_name := oldname ;
      raise e
    | ast ->
      Location.input_name := oldname ;
      ast in
  ast, content


let emit out_chan name =
  output_string out_chan "#1 \"";
  (*Note here we do this is mostly to avoid leaking user's
    information, like private path, in the future, we can have
    a flag
  *)
  output_string out_chan (Filename.basename name) ;
  output_string out_chan "\"\n"

let decorate_module 
    ?(module_bound=true)
    out_chan base mli_name ml_name mli_content ml_content =
  if module_bound then begin 
    let base = String.capitalize base in
    output_string out_chan "module ";
    output_string out_chan base ;
    output_string out_chan " : sig \n";
    emit out_chan mli_name ;
    preprocess_string mli_name mli_content out_chan;
    output_string out_chan "\nend = struct\n";
    emit out_chan  ml_name ;
    preprocess_string ml_name ml_content out_chan;
    output_string out_chan "\nend\n"
  end
  else
    begin 
      output_string out_chan "include (struct\n";
      emit out_chan  ml_name ;
      preprocess_string ml_name ml_content out_chan;
      output_string out_chan "\nend : sig \n";
      emit out_chan mli_name ;
      preprocess_string mli_name mli_content out_chan;
      output_string out_chan "\nend)";
    end

let decorate_module_only 
    ?(module_bound=true) 
    out_chan base ml_name ml_content =
  if module_bound then begin 
    let base = String.capitalize base in
    output_string out_chan "module ";
    output_string out_chan base ;
    output_string out_chan "\n= struct\n"
  end;
  emit out_chan  ml_name;
  preprocess_string ml_name ml_content out_chan ; 
  (* output_string out_chan ml_content; *)
  if module_bound then 
    output_string out_chan "\nend\n"

(** recursive module is not good for performance, here module type only 
    has to be pure types otherwise it would not compile any way
*)
let decorate_interface_only out_chan  base  mli_name mli_content =
  output_string out_chan "(** Interface as module  *)\n";
  decorate_module_only out_chan base mli_name mli_content

(** set mllib *)
let mllib = ref None
let set_string s = mllib := Some s

let batch_files = ref []
let collect_file name =
  batch_files := name :: !batch_files

let output_file = ref None
let set_output file = output_file := Some file
let header_option = ref false

type main_module = { modulename : string ; export : bool }

(** set bs-main*)
let main_module : main_module option ref = ref None

let set_main_module modulename = 
  main_module := Some {modulename; export = false }

let set_main_export modulename = 
  main_module := Some {modulename; export = true }

let set_mllib_file = ref false     

let prelude = ref None 
let set_prelude f = 
  if Sys.file_exists f then 
    prelude := Some f 
  else raise (Arg.Bad ("file " ^ f ^ " don't exist "))
let prelude_str = ref None
let set_prelude_str f = prelude_str := Some f 

(**
   {[
     # process_include "ghsogh?a,b,c";;
     - : [> `Dir of string | `Dir_with_excludes of string * string list ] =
     `Dir_with_excludes ("ghsogh", ["a"; "b"; "c"])
                        # process_include "ghsogh?a";;
     - : [> `Dir of string | `Dir_with_excludes of string * string list ] =
     `Dir_with_excludes ("ghsogh", ["a"])
   ]}
*)
(* type dir_spec =  *)
(*   [ `Dir of string | `Dir_with_excludes of string * string list ] *)

let cwd = Sys.getcwd ()

let normalize s = 
  Ext_path.normalize_absolute_path (Ext_path.combine cwd s )

let process_include s : Ast_extract.dir_spec = 
  let i = Ext_string.rindex_neg s '?'   in 
  if i < 0 then   
    { dir = normalize s; excludes = []}
  else 
    let dir = String.sub s 0 i in
    { dir = normalize dir;
      excludes = Ext_string.split 
          (String.sub s (i + 1) (String.length s - i - 1)    )
          ','}

let deduplicate_dirs (xs : Ast_extract.dir_spec list) =
  let set :  Ast_extract.dir_spec String_hashtbl.t = String_hashtbl.create 64 in 
  List.filter (fun ({Ast_extract.dir ; excludes = new_excludes } as y) -> 
      match String_hashtbl.find_opt set dir with
      | None ->  
        String_hashtbl.add set dir y;
        true 
      | Some x ->  x.excludes <- new_excludes @ x.excludes ; false
    ) xs 

let includes :  _ list ref = ref []

let add_include dir = 
  includes := process_include    dir :: !includes

let exclude_modules = ref []                                     
let add_exclude module_ = 
  exclude_modules := module_ :: !exclude_modules
let no_implicit_include = ref false 

let alias_map = String_hashtbl.create 0
let alias_map_rev = String_hashtbl.create 0

(**
   {[
     A -> B 
       A1 -> B
   ]}
   print 
   {[

     module A = B
     module A1 = B  
   ]}   
   We don't suppport 
   {[
     A -> B 
       A -> C
   ]}
*)    
let alias_module s = 
  match Ext_string.split s '=' with 
  | [a;b] -> 
    (* Error checking later*)
    if String_hashtbl.mem alias_map a then 
      raise (Arg.Bad ("duplicated module alias " ^ a))
    else
      begin 
        String_hashtbl.add alias_map_rev b a;
        String_hashtbl.add alias_map a b 
      end
  | _ -> raise (Arg.Bad "invalid module alias format like A=B")

let undefine_symbol (s : string) = 
  Lexer.remove_directive_built_in_value s 
let define_symbol (s : string) = 
  match Ext_string.split ~keep_empty:true s '=' with
  | [key; v] -> 
    if not @@ Lexer.define_key_value key v  then 
      raise (Arg.Bad ("illegal definition: " ^ s))
  | _ -> raise (Arg.Bad ("illegal definition: " ^ s))

let specs : (string * Arg.spec * string) list =
  [ 
    "-bs-no-implicit-include", (Arg.Set no_implicit_include),
    " Not including cwd as search path";
    "-prelude-str", (Arg.String set_prelude_str),
    " Set a prelude string, (before -prelude) option" ;
    "-module-alias", (Arg.String alias_module ),
    " -module-alis A=B, whenever need A,replace it with B" ;
    "-prelude", (Arg.String set_prelude),
    " Set a prelude file, literally copy into the beginning";    
    "-bs-mllib", (Arg.String set_string),
    " Files collected from mllib";
    "-bs-MD", (Arg.Set set_mllib_file),
    " Log files into mllib(only effective under -bs-main mode)";
    "-o", (Arg.String set_output),
    " Set output file (default to stdout)" ;
    "-with-header", (Arg.Set header_option),
    " with header of time stamp" ; 
    "-bs-exclude-I", (Arg.String add_exclude),
    " don't read and pack such modules from -I (in the future, we should detect conflicts in mllib or commandline) "
    ;
    "-bs-main", (Arg.String set_main_module),
    " set the main entry module";
    "-main-export", (Arg.String set_main_export),
    " Set the main module and respect its exports";
    "-I",  (Arg.String add_include),
    " add dir to search path";
    "-U", Arg.String undefine_symbol,
    " Undefine a symbol when bspacking";
    "-D", Arg.String define_symbol, 
    " Define a symbol when bspacking"
  ]


let anonymous filename =
  collect_file filename

let usage = "Usage: bspack <options> <files>\nOptions are:"
let () =
  try
    (Arg.parse specs anonymous usage;
     let command_files =  !batch_files in
     let mllib = !mllib in 
     (* emit code now *)
     let out_chan =
       lazy (match !output_file with
           | None -> stdout
           | Some file -> open_out_bin file)  in
     let emit_header out_chan = 
       let local_time = Unix.(localtime (gettimeofday ())) in
       (if  !header_option 
        then
          output_string out_chan
            (Printf.sprintf "(** Generated by bspack %02d/%02d-%02d:%02d *)\n"
               (local_time.tm_mon + 1) local_time.tm_mday
               local_time.tm_hour local_time.tm_min));
       (match !prelude_str with 
        | None -> ()
        | Some s -> output_string out_chan s; output_string out_chan "\n" );
       match !prelude with
       | None -> ()
       | Some f -> 
         begin 
           output_string out_chan (Ext_io.load_file f);
           output_string out_chan "\n"         
         end
     in
     let close_out_chan out_chan = 
       (if  out_chan != stdout then close_out out_chan) in
     let files =
       (match mllib with
        | Some s
          -> read_lines (Sys.getcwd ()) s
        | None -> []) @ command_files in

     match !main_module, files with
     | Some _ , _ :: _
       -> 
       Ext_pervasives.failwithf ~loc:__LOC__ 
         "-bs-main conflicts with other flags [ %s ]"
         (String.concat ", " files)
     | Some {modulename  = main_module ; export },  []
       ->
       let excludes =
         match !exclude_modules with
         | [] -> []
         | xs -> 
           Ext_list.flat_map (fun x -> [x ^ ".ml" ; x ^ ".mli"] ) xs in 
       let extra_dirs = 
         deduplicate_dirs @@
         if not !no_implicit_include then {Ast_extract.dir =  cwd; excludes = []} :: !includes 
         else !includes 
       in  
       let ast_table, tasks =
         Ast_extract.collect_from_main ~excludes ~extra_dirs ~alias_map
           Format.err_formatter
           (fun _ppf sourcefile -> lazy (implementation sourcefile))
           (fun _ppf sourcefile -> lazy (interface sourcefile))
           (fun (lazy (stru, _)) -> stru)
           (fun (lazy (sigi, _)) -> sigi)
           main_module
       in 
       if Queue.is_empty tasks then 
         raise (Arg.Bad (main_module ^ " does not pull in any libs, maybe wrong input"))
       ;
       let out_chan = Lazy.force out_chan in
       let collect_modules  = !set_mllib_file in 
       let collection_modules = Queue.create () in
       let count = ref 0 in 
       let task_length = Queue.length tasks in 
       emit_header out_chan ;
       begin 
         Ast_extract.handle_queue Format.err_formatter tasks ast_table
           (fun base ml_name (lazy(_, ml_content)) -> 
              incr count ;  
              if collect_modules then 
                Queue.add ml_name collection_modules; 
              let module_bound = not  export || task_length > !count  in 
              decorate_module_only ~module_bound out_chan base ml_name ml_content;
              let aliased = String.capitalize base in 
              String_hashtbl.find_all alias_map_rev aliased
              |> List.iter 
                (fun s -> output_string out_chan (Printf.sprintf "module %s = %s \n"  s aliased))

           )
           (fun base mli_name (lazy (_, mli_content))  -> 
              incr count ;                  
              if collect_modules then 
                Queue.add mli_name collection_modules;                 

              decorate_interface_only out_chan base mli_name mli_content;
              let aliased = String.capitalize base in 
              String_hashtbl.find_all alias_map_rev aliased
              |> List.iter 
                (fun s -> output_string out_chan (Printf.sprintf "module %s = %s \n"  s aliased))

           )
           (fun base mli_name ml_name (lazy (_, mli_content)) (lazy (_, ml_content))
             -> 
               incr count;  
               (*TODO: assume mli_name, ml_name are in the same dir,
                 Needs to be addressed 
               *)
               if collect_modules then 
                 begin 
                   Queue.add ml_name collection_modules;
                   Queue.add mli_name collection_modules
                 end; 
               (** if export 
                   print it as 
                   {[inclue (struct end : sig end)]}
               *)   
               let module_bound = not export || task_length > !count in 
               decorate_module ~module_bound out_chan base mli_name ml_name mli_content ml_content;
               let aliased = (String.capitalize base) in 
               String_hashtbl.find_all alias_map_rev aliased
               |> List.iter 
                 (fun s -> output_string out_chan (Printf.sprintf "module %s = %s \n"  s aliased))

           )
       end;
       close_out_chan out_chan;
       begin 
         if !set_mllib_file then
           match !output_file with
           | None -> ()
           | Some file ->
             let output = (Ext_path.chop_extension_if_any file ^ ".d") in
             let sorted_queue = 
               Queue.fold (fun acc x -> String_set.add x acc) String_set.empty  collection_modules in 
             Ext_io.write_file 
               output
               (
                 (* Queue.fold *)
                 String_set.fold
                   (fun a acc  -> 
                      acc ^ file ^ " : " ^ 
                      (*FIXME: now we normalized path,
                        we need a beautiful output too for relative path
                        The relative path should be also be normalized..
                      *)
                      Filename.concat 
                        (Ext_path.rel_normalized_absolute_path
                           ~from:cwd 
                           (Filename.dirname a)
                        ) (Filename.basename a)

                      ^ "\n"
                      (* ^ a ^ " : ; touch " ^ output ^ "\n" *)
                   ) sorted_queue
                   Ext_string.empty 
                   (* collection_modules *)
               )
       end
     | None, _ -> 
       let ast_table =
         Ast_extract.collect_ast_map
           Format.err_formatter files
           (fun _ppf sourcefile -> implementation sourcefile
           )
           (fun _ppf sourcefile -> interface sourcefile) in
       let tasks = Ast_extract.sort fst  fst ast_table in
       let out_chan = (Lazy.force out_chan) in
       emit_header out_chan ;
       Ast_extract.handle_queue Format.err_formatter tasks ast_table 
         (fun base ml_name (_, ml_content) -> decorate_module_only  out_chan base ml_name ml_content)
         (fun base mli_name (_, mli_content)  -> decorate_interface_only out_chan base mli_name mli_content )
         (fun base mli_name ml_name (_, mli_content) (_, ml_content)
           -> decorate_module out_chan base mli_name ml_name mli_content ml_content);
       close_out_chan out_chan
    )
  with x ->
    begin
      Location.report_exception Format.err_formatter x ;
      exit 2
    end
