(***
   This test evaluate boolean formula composed by conjunction and
     disjunction using ephemeron:
   - true == alive, false == garbage collected
   - and == an n-ephemeron, or == many 1-ephemeron

*)

let nb_test = 4
let max_level = 10
 (** probability that a branch is not linked to a previous one *)
let proba_no_shared = 0.2
let arity_max = 4

let proba_new = proba_no_shared ** (1./.(float_of_int max_level))

open Format
open Ephemeron

let is_true test s b = printf "%s %s: %s\n" test s (if b then "OK" else "FAIL")
let is_false test s b = is_true test s (not b)

type varephe = int ref
type ephe = (varephe,varephe) Kn.t

type formula =
  | Constant of bool
  | And of var array
  | Or of var array

and var = {
  form: formula;
  value: bool;
  ephe: varephe Weak.t;
}

let print_short_bool fmt b =
  if b
  then pp_print_string fmt "t"
  else pp_print_string fmt "f"

let rec pp_form fmt = function
  | Constant b ->
      fprintf fmt "%B" b
  | And a      ->
      fprintf fmt "And[@[%a@]]" (fun fmt -> Array.iter (pp_var fmt)) a
  | Or a       ->
      fprintf fmt  "Or[@[%a@]]" (fun fmt -> Array.iter (pp_var fmt)) a

and pp_var fmt v =
  fprintf fmt "%a%a:%a;@ "
    print_short_bool v.value
    print_short_bool (Weak.check v.ephe 0)
    pp_form v.form

type env = {
  (** resizeable array for cheap *)
  vars : (int,var) Hashtbl.t;
  (** the ephemerons must be alive *)
  ephes : ephe Stack.t;
  (** keep alive the true constant *)
  varephe_true : varephe Stack.t;
(** keep temporarily alive the false constant *)
  varephe_false : varephe Stack.t;
}

let new_env () = {
  vars = Hashtbl.create 100;
  ephes = Stack.create ();
  varephe_true = Stack.create ();
  varephe_false = Stack.create ();
}

let evaluate = function
  | Constant b -> b
  | And a -> Array.fold_left (fun acc e -> acc && e.value) true  a
  | Or a  -> Array.fold_left (fun acc e -> acc || e.value) false a

let get_ephe v =
  match Weak.get v.ephe 0 with
  | None ->
      invalid_arg "Error: weak dead but nothing have been released"
  | Some r -> r

(** create a variable and its definition in the boolean world and
    ephemerons world *)
let rec create env rem_level (** remaining level *) =
  let varephe = ref 1 in
  let form =
    if rem_level = 0 then (** Constant *)
      if Random.bool ()
      then (Stack.push varephe env.varephe_true ; Constant true )
      else (Stack.push varephe env.varephe_false; Constant false)
    else
      let size = (Random.int (arity_max - 1)) + 2 in
      let new_link _ =
        if (Hashtbl.length env.vars) = 0 || Random.float 1. < proba_new
        then create env (rem_level -1)
        else Hashtbl.find env.vars (Random.int (Hashtbl.length env.vars))
      in
      let args = Array.init size new_link in
      if Random.bool ()
      then begin (** Or *)
        Array.iter (fun v ->
            let r = get_ephe v in
            let e = Kn.create 1 in
            Kn.set_key e 0 r;
            Kn.set_data e varephe;
            Stack.push e env.ephes
          ) args; Or args
      end
      else begin (** And *)
        let e = Kn.create (Array.length args) in
        for i=0 to Array.length args - 1 do
          Kn.set_key e i (get_ephe args.(i));
        done;
        Kn.set_data e varephe;
        Stack.push e env.ephes;
        And args
      end
  in
  let create_weak e =
    let w = Weak.create 1 in
    Weak.set w 0 (Some e);
    w
  in
  let v = {form; value = evaluate form;
           ephe = create_weak varephe;
          } in
  Hashtbl.add env.vars (Hashtbl.length env.vars) v;
  v


let check_var v = v.value = Weak.check v.ephe 0

let run test init =
  Random.init init;
  let env = new_env () in
  let _top = create env max_level in
  (** release false ref *)
  Stack.clear env.varephe_false;
  Gc.full_major ();
  let res = Hashtbl.fold (fun _ v acc -> acc && check_var v) env.vars true in
  is_true test "check" res

let () =
  for i = 0 to nb_test do
    run ("test"^(string_of_int i)) i;
  done
