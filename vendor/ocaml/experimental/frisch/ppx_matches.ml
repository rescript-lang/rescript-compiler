(*
  Example : List.filter [%matches ? 'a' .. 'z' ] text
  Output : List.filter  (function 'a' .. 'z' -> true | _ -> false) text
*)

open Asttypes
open Parsetree
open Ast_helper

let mapper _args =
  let open Ast_mapper in
  let super = default_mapper in
  {super with
   expr =
     (fun this e ->
        match e.pexp_desc with
        | Pexp_extension({txt="matches";_}, PPat (p, guard)) ->
            let p = this.pat this p in
            let guard = Ast_mapper.map_opt (this.expr this) guard in
            Exp.function_ ~loc:e.pexp_loc
              [
            Exp.case p ?guard (Convenience.constr "true" []);
            Exp.case (Pat.any ()) (Convenience.constr "false" []);
              ]
        | _ -> super.expr this e
     )
  }

let () = Ast_mapper.run_main mapper
