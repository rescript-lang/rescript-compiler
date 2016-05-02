module C = Stack
let read_parse_and_extract ast extract_function =
  Depend.free_structure_names := Depend.StringSet.empty;
  (let bound_vars = Depend.StringSet.empty in
  List.iter
    (fun modname  ->
      Depend.open_module bound_vars (Longident.Lident modname))
    (!Clflags.open_modules);
  extract_function bound_vars ast;
  !Depend.free_structure_names)
type file_kind =
  | ML
  | MLI
let files = ref []
let ml_file_dependencies ((source_file : string),ast) =
  let extracted_deps =
    read_parse_and_extract ast Depend.add_implementation in
  files := ((source_file, ML, extracted_deps) :: (!files))
let mli_file_dependencies (source_file,ast) =
  let extracted_deps =
    read_parse_and_extract ast Depend.add_signature in
  files := ((source_file, MLI, extracted_deps) :: (!files))
let normalize file  =
  let modname = String.capitalize 
      (Filename.chop_extension @@ Filename.basename file) in
  modname

let merge (files : (string * file_kind * Depend.StringSet.t) list ) =
  let tbl = Hashtbl.create 31 in 

  let domain = Depend.StringSet.of_list 
      (List.map (fun (x,_,_)-> normalize x) files) in
  let () = List.iter (fun  (file,file_kind,deps) ->
    let modname = String.capitalize 
        (Filename.chop_extension @@ Filename.basename file) in
    match Hashtbl.find tbl modname with 
    | new_deps -> Hashtbl.replace tbl modname 
          (Depend.StringSet.inter domain (Depend.StringSet.union deps new_deps))
    | exception Not_found -> 
        Hashtbl.add tbl  modname (Depend.StringSet.inter deps domain)
                     ) files  in tbl


let sort_files_by_dependencies 
    (files : (string * file_kind * Depend.StringSet.t) list)
    =
  let h : (string, Depend.StringSet.t) Hashtbl.t = merge files in
  let () = 
    begin
      (* prerr_endline "dumping dependency table"; *)
      (* Hashtbl.iter (fun key _ -> prerr_endline key) h ; *)
      (* prerr_endline "dumping dependency finished"; *)
    end in
  let worklist = Stack.create () in
  let ()= 
    Hashtbl.iter (fun key _     -> Stack.push key worklist ) h in
  let result = C.create () in
  let visited = Hashtbl.create 31 in

  while not @@ Stack.is_empty worklist do 
    (* let () =  *)
    (*   prerr_endline "stack ..."; *)
    (*   Stack.iter (fun x -> prerr_string x ; prerr_string" ") worklist ; *)
    (*   prerr_endline "stack ...end" *)
    (* in *)
    let current = Stack.top worklist  in 
    if Hashtbl.mem visited current then
      ignore @@ Stack.pop worklist 
    else 
      match Depend.StringSet.elements (Hashtbl.find h current) with 
      | depends -> 
          let really_depends = 
            List.filter (fun x ->  (Hashtbl.mem h x && (not (Hashtbl.mem visited x ))))
              depends in
          begin match really_depends with 
          |[] -> begin
              let v = Stack.pop worklist in
              Hashtbl.add visited  v () ;
              (* prerr_endline (Printf.sprintf "poping %s" v); *)
              C.push current result 
          end
          | _ -> 
              List.iter  (fun x -> Stack.push x worklist) really_depends
          end
      | exception Not_found ->  assert false 
            (* prerr_endline current; *)
            (* Hashtbl.iter (fun k _ -> prerr_endline k) h ; *)
            (* failwith current *)
  done;
  result
;;

let _loc = Location.none 

let assemble  ast_tbl  stack = 
  let structure_items = ref [] in
  let visited = Hashtbl.create 31 in
  Stack.iter
    (fun base  ->
      match Hashtbl.find visited base with 
      | exception Not_found ->
          Hashtbl.add visited base ();
          begin match Hashtbl.find_all ast_tbl base with
            | (`ml (structure, _))::(`mli (signature, _))::[]
            |(`mli (signature, _))::(`ml (structure, _))::[] ->
              let v: Parsetree.structure_item =
                {
                  Parsetree.pstr_loc = _loc;
                  pstr_desc =
                    (Pstr_module
                       {
                         pmb_name =
                           { txt = (String.capitalize base); loc = _loc
                           };
                         pmb_expr =
                           {
                             pmod_desc =
                               (Pmod_constraint
                                  ({
                                    pmod_desc =
                                      (Pmod_structure structure);
                                    pmod_loc = _loc;
                                    pmod_attributes = []
                                  },
                                    ({
                                      pmty_desc =
                                        (Pmty_signature signature);
                                      pmty_loc = _loc;
                                      pmty_attributes = []
                                    } : Parsetree.module_type)));
                             pmod_loc = _loc;
                             pmod_attributes = []
                           };
                         pmb_attributes = [];
                         pmb_loc = _loc
                       })
                } in
              structure_items := (v :: (!structure_items))
            | (`ml (structure, _))::[] ->
              let v: Parsetree.structure_item =
                {
                  Parsetree.pstr_loc = _loc;
                  pstr_desc =
                    (Pstr_module
                       {
                         pmb_name =
                           { txt = (String.capitalize base); loc = _loc
                           };
                         pmb_expr =
                           {
                             pmod_desc = (Pmod_structure structure);
                             pmod_loc = _loc;
                             pmod_attributes = []
                           };
                         pmb_attributes = [];
                         pmb_loc = _loc
                       })
                } in
              structure_items := (v :: (!structure_items))

            | _ -> assert false
          end
      | _ -> () 
    ) stack;
  {
    Parsetree.pstr_loc = _loc;
    pstr_desc =
      (Pstr_include
         {
           pincl_mod =
             {
               pmod_desc = (Pmod_structure ( !structure_items));
               pmod_loc = _loc;
               pmod_attributes = []
             };
           pincl_loc = _loc;
           pincl_attributes = []
         })
  }

let process arg_files  : Parsetree.structure_item =

  let ast_tbl = Hashtbl.create 31 in
  let files_set = Depend.StringSet.of_list @@ arg_files in
  let () = files_set |> Depend.StringSet.iter (fun name ->
    
    let chan = open_in name in
    let lexbuf = Lexing.from_channel chan in
    let base = normalize name in
    if Filename.check_suffix name ".ml"
    then
      let ast = Parse.implementation lexbuf in
      (Hashtbl.add ast_tbl base (`ml (ast , name));
       ml_file_dependencies (name, ast))
    else
      if Filename.check_suffix name ".mli"
      then
        (if Depend.StringSet.mem  (Filename.chop_extension name ^ ".ml") files_set then
          match Parse.interface lexbuf with 
          | ast -> 
              Hashtbl.add ast_tbl base (`mli (ast, name));
              mli_file_dependencies (name, ast)
          | exception _ -> failwith (Printf.sprintf "failed parsing %s" name)
        else
          begin match Parse.interface lexbuf with 
          | ast -> 
              (* prerr_endline name; *)
              Hashtbl.add ast_tbl base (`mli (ast, name));
              mli_file_dependencies (name, ast);
              seek_in chan 0 ; 
              let lexbuf = Lexing.from_channel chan in 
              begin match  Parse.implementation lexbuf with
              | impl ->
                  Hashtbl.add ast_tbl base (`ml (impl, name));
                  ml_file_dependencies 
                    (name, impl) (* Fake*)
              | exception _ -> failwith (Printf.sprintf "failed parsing %s as ml" name) 
              end
          | exception _  -> 
              failwith (Printf.sprintf "failed parsing %s" name) 
          end
        )
      else assert false) in
  
  assemble ast_tbl (sort_files_by_dependencies (!files))
  


(**
   known issues:
   we take precedence of ml seriously, however, there is a case 
   1. module a does not depend on b 
   while interface a does depend on b 
   in this case, we put a before b which will cause a compilation error 
   (this does happens when user use polymorphic variants in the interface while does not refer module b in the implementation, while the interface does refer module b)

   2. if we only take interface seriously, first the current worklist algorithm does not provide 
   [same level ] information, second the dependency captured by interfaces are very limited.
   3. The solution would be combine the dependency of interfaces and implementations altogether, 
   we will get rid of some valid use cases, but it's worth 
   
 *)
(* local variables: *)
(* compile-command: "ocamlopt.opt -inline 1000 -I +compiler-libs -c depend.ml ocaml_extract.mli ocaml_extract.ml " *)
(* end: *)
