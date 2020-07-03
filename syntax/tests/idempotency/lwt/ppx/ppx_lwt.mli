(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Ppx syntax extension for Lwt *)


(** {2 Ppx extensions}

    This Ppx extension adds various syntactic shortcut for lwt programming.
    It needs OCaml >= 4.02 and {{:https://github.com/alainfrisch/ppx_tools}ppx_tools}.

    To use it, simply use the ocamlfind package [lwt_ppx].

   This extension adds the following syntax:

   - lwt-binding:

   {[
let%lwt ch = get_char stdin in
code
   ]}

   is the same as [bind (get_char stdin) (fun ch -> code)].

   Moreover, it supports parallel binding:

   {[
let%lwt x = do_something1 ()
and y = do_something2 in
code
   ]}

   will run [do_something1 ()] and [do_something2 ()], then
   bind their results to [x] and [y]. It is the same as:

   {[
let t1 = do_something1
and t2 = do_something2 in
bind t1 (fun x -> bind t2 (fun y -> code))
   ]}

   Due to a {{:https://caml.inria.fr/mantis/view.php?id=7758} bug} in the OCaml
   parser, if you'd like to put a type constraint on the variable, please write

   {[
let (foo : int) = do_something in
code
   ]}

   Not using parentheses will confuse the OCaml parser.

   - exception catching:

   {[
try%lwt
  <expr>
with
  <branches>
   ]}

   For example:

   {[
try%lwt
  f x
with
  | Failure msg ->
      prerr_endline msg;
      return ()
   ]}

   is expanded to:

   {[
catch (fun () -> f x)
  (function
    | Failure msg ->
        prerr_endline msg;
        return ()
    | exn ->
        Lwt.fail exn)
   ]}

   Note that the [exn -> Lwt.fail exn] branch is automatically added
   when needed.

   - finalizer:

   {[
     (<expr>) [%finally <expr>]
   ]}

   You can use [[%lwt.finally ...]] instead of [[%finally ...]].


   - assertion:

   {[
     assert%lwt <expr>
   ]}

   - for loop:

   {[
for%lwt i = <expr> to <expr> do
  <expr>
done
   ]}

   and:

   {[
for%lwt i = <expr> downto <expr> do
  <expr>
done
   ]}

   - while loop:

   {[
while%lwt <expr> do
  <expr>
done
   ]}

   - pattern matching:

   {[
match%lwt <expr> with
  | <patt_1> -> <expr_1>
      ...
  | <patt_n> -> <expr_n>
   ]}

   Exception cases are also supported:

   {[
match%lwt <expr> with
  | exception <exn> -> <expr_1>
  | <patt_2> -> <expr_2>
      ...
  | <patt_n> -> <expr_n>
   ]}

   - conditional:

   {[
if%lwt <expr> then
  <expr_1>
else
  <expr_2>
   ]}

   and

   {[
     if%lwt <expr> then <expr_1>
   ]}
*)


val mapper : Migrate_parsetree.OCaml_411.Ast.Ast_mapper.mapper
