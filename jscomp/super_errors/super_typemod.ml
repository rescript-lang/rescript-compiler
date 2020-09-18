open Printtyp

let fprintf = Format.fprintf

let non_generalizable_msg ppf print_fallback_msg =
  fprintf ppf
    "%a@,@,\
    @[This happens when the type system senses there's a mutation/side-effect,@ in combination with a polymorphic value.@,\
    @{<info>Using or annotating that value usually solves it.@}@]"
  print_fallback_msg ()

(* taken from https://github.com/rescript-lang/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typemod.ml#L1754 *)
(* modified branches are commented *)
let report_error ppf = function
  | Typemod.Non_generalizable typ ->
      (* modified *)
      fprintf ppf "@[<v>";
      non_generalizable_msg
        ppf
        (fun ppf () ->
          fprintf ppf
          "@[This expression's type contains type variables that can't be generalized:@,@{<error>%a@}@]"
          type_scheme typ);
      fprintf ppf "@]"
  | Non_generalizable_module mty ->
      (* modified *)
      fprintf ppf "@[<v>";
      non_generalizable_msg
        ppf
        (fun ppf () ->
          fprintf ppf
            "@[The type of this module contains type variables that cannot be generalized:@,@{<error>%a@}@]"
            modtype mty);
        fprintf ppf "@]"
  | anythingElse ->
      Typemod.super_report_error_no_wrap_printing_env ppf anythingElse

let report_error env ppf err =
  Printtyp.wrap_printing_env env (fun () -> report_error ppf err)

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typemod.Error (loc, env, err) ->
        Some (Super_location.error_of_printer loc (report_error env) err)
      | Typemod.Error_forward err ->
        Some err
      | _ ->
        None
    )
