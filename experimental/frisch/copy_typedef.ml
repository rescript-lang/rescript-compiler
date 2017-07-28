(*
  A -ppx rewriter to copy type definitions from the interface into
  the implementation.

  In an .ml file, you can write:

    type t = [%copy_typedef]

  and the concrete definition will be copied from the corresponding .mli
  file (looking for the type name in the same path).

  The same is available for module types:

    module type S = [%copy_typedef]

  You can also import a definition from an arbitrary .ml/.mli file.
  Example:

   type loc = [%copy_typedef "../../parsing/location.mli" t]

  Note: the definitions are imported textually without any substitution.
*)

module Main : sig end = struct
  open Asttypes
  open! Location
  open Parsetree

  let fatal loc s =
    Location.print_error Format.err_formatter loc;
    prerr_endline ("** copy_typedef: " ^ Printexc.to_string s);
    exit 2

  class maintain_path = object(this)
    inherit Ast_mapper.mapper as super

    val path = []

    method! module_binding m = {< path = m.pmb_name.txt :: path >} # super_module_binding m
    method super_module_binding = super # module_binding

    method! module_declaration m = {< path = m.pmd_name.txt :: path >} # super_module_declaration m
    method super_module_declaration = super # module_declaration

    method! module_type_declaration m = {< path = m.pmtd_name.txt :: path >} # super_module_type_declaration m
    method super_module_type_declaration = super # module_type_declaration

    method! structure_item s =
      let s =
        match s.pstr_desc with
        | Pstr_type tdecls -> {s with pstr_desc=Pstr_type (List.map (this # tydecl) tdecls)}
        | Pstr_modtype mtd -> {s with pstr_desc=Pstr_modtype (this # mtydecl mtd)}
        | _ -> s
      in
      super # structure_item s

    method! signature_item s =
      let s =
        match s.psig_desc with
        | Psig_type tdecls -> {s with psig_desc=Psig_type (List.map (this # tydecl) tdecls)}
        | Psig_modtype mtd -> {s with psig_desc=Psig_modtype (this # mtydecl mtd)}
        | _ -> s
      in
      super # signature_item s

    method tydecl x = x
    method mtydecl x = x
  end

  let memoize f =
    let h = Hashtbl.create 16 in
    fun x ->
      try Hashtbl.find h x
      with Not_found ->
        let r = f x in
        Hashtbl.add h x r;
        r

  let from_file file =
    let types = Hashtbl.create 16 in
    let mtypes = Hashtbl.create 16 in
    let collect = object
      inherit maintain_path
      method! tydecl x =
        Hashtbl.add types (path, x.ptype_name.txt) x;
        x
      method! mtydecl x =
        Hashtbl.add mtypes (path, x.pmtd_name.txt) x;
        x
    end
    in
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    if Filename.check_suffix file ".ml"
    then ignore (collect # structure (Parse.implementation lexbuf))
    else if Filename.check_suffix file ".mli"
    then ignore (collect # signature (Parse.interface lexbuf))
    else failwith (Printf.sprintf "Unknown extension for %s" file);
    close_in ic;
    object
      method tydecl path name =
        try Hashtbl.find types (path, name)
        with Not_found ->
          failwith
            (Printf.sprintf "Cannot find type %s in file %s\n%!"
               (String.concat "." (List.rev (name :: path))) file)

      method mtydecl path name =
        try Hashtbl.find mtypes (path, name)
        with Not_found ->
          failwith
            (Printf.sprintf "Cannot find module type %s in file %s\n%!"
               (String.concat "." (List.rev (name :: path))) file)
    end

  let from_file = memoize from_file

  let copy = object(this)
    inherit maintain_path as super

    val mutable file = ""

    method source name = function
      | PStr [] ->
          let file =
            if Filename.check_suffix file ".ml"
            then (Filename.chop_suffix file ".ml") ^ ".mli"
            else if Filename.check_suffix file ".mli"
            then (Filename.chop_suffix file ".mli") ^ ".ml"
            else failwith "Unknown source extension"
          in
          file, path, name
      | PStr [{pstr_desc=Pstr_eval
            ({pexp_desc=Pexp_apply
                ({pexp_desc=Pexp_constant(Const_string (file, _)); _},
                 ["", {pexp_desc=Pexp_ident{txt=lid;_}; _}]); _}, _); _}] ->
          begin match List.rev (Longident.flatten lid) with
          | [] -> assert false
          | name :: path -> file, path, name
          end
      | _ ->
          failwith "Cannot parse argument"

    method! tydecl = function
      | {ptype_kind = Ptype_abstract;
         ptype_manifest =
         Some{ptyp_desc=Ptyp_extension({txt="copy_typedef";_}, arg); _};
         ptype_name = name; ptype_loc = loc; _
        } ->
          begin try
            let (file, path, x) = this # source name.txt arg in
            {((from_file file) # tydecl path x)
            with ptype_name = name; ptype_loc = loc}
          with exn -> fatal loc exn
          end
      | td -> td

    method! mtydecl = function
      | {pmtd_type = Some{pmty_desc=Pmty_extension({txt="copy_typedef";_}, arg);
                          pmty_loc=loc; _};
         pmtd_name = name; _
        } ->
          begin try
            let (file, path, x) = this # source name.txt arg in
            {((from_file file) # mtydecl path x)
            with pmtd_name = name}
          with exn -> fatal loc exn
          end
      | td -> td

    method! implementation f x =
      file <- f;
      super # implementation f x

    method! interface f x =
      file <- f;
      super # interface f x
  end

  let () = Ast_mapper.main copy
end
