(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Berke Durak *)
(* Glob *)
open My_std;;
open Bool;;
include Glob_ast;;
open Glob_lexer;;

let sf = Printf.sprintf;;

let brute_limit = 10;;

(*** string_of_token *)
let string_of_token = function
| ATOM _ -> "ATOM"
| AND -> "AND"
| OR -> "OR"
| NOT -> "NOT"
| LPAR -> "LPAR"
| RPAR -> "RPAR"
| TRUE -> "TRUE"
| FALSE -> "FALSE"
| EOF -> "EOF"
;;
(* ***)
(*** match_character_class *)
let match_character_class cl c =
  Bool.eval
    begin function (c1,c2) ->
      c1 <= c && c <= c2
    end
    cl
;;
(* ***)
(*** NFA *)
module NFA =
  struct
    type transition =
    | QCLASS of character_class
    | QEPSILON
    ;;

    module IS = Set.Make(struct type t = int let compare (x:t) y = compare x y let print = Format.pp_print_int end);;
    module ISM = Map.Make(struct type t = IS.t let compare = IS.compare let print = IS.print end);;

    type machine = {
      mc_qi : IS.t;
      mc_table : (character_class * IS.t) list array;
      mc_qf : int;
      mc_power_table : (char, IS.t ISM.t) Hashtbl.t
    }

    (*** build' *)
    let build' p =
      let count = ref 0 in
      let transitions = ref [] in
      let epsilons : (int * int) list ref = ref [] in
      let state () = let id = !count in incr count; id in
      let ( --> ) q1 t q2 =
        match t with
        | QEPSILON -> epsilons := (q1,q2) :: !epsilons; q1
        | QCLASS cl -> transitions := (q1,cl,q2) :: !transitions; q1
      in
      (* Construit les transitions correspondant au motif donne et arrivant
       * sur l'etat qf.  Retourne l'etat d'origine. *)
      let rec loop qf = function
        | Epsilon  -> qf
        | Word u   ->
            let m = String.length u in
            let q0 = state () in
            let rec loop q i =
              if i = m then
                q0
              else
                begin
                  let q' =
                    if i = m - 1 then
                      qf
                    else
                      state ()
                  in
                  let _ = (q --> QCLASS(Atom(u.[i], u.[i]))) q' in
                  loop q' (i + 1)
                end
            in
            loop q0 0
        | Class cl ->
            let q1 = state () in
            (q1 --> QCLASS cl) qf
        | Star p ->
            (* The fucking Kleene star *)
            let q2 = state () in
            let q1 = loop q2 p in (* q1 -{p}-> q2 *)
            let _ = (q1 --> QEPSILON) qf in
            let _ = (q2 --> QEPSILON) q1 in
            let _ = (q2 --> QEPSILON) q1 in
            q1
        | Concat(p1,p2) ->
            let q12 = state () in
            let q1  = loop q12 p1 in (* q1  -{p1}-> q12 *)
            let q2  = loop qf  p2 in (* q2  -{p2}-> qf *)
            let _   = (q12 --> QEPSILON) q2 in
            q1
        | Union pl ->
            let qi = state () in
            List.iter
              begin fun p ->
                let q = loop qf p in           (* q -{p2}-> qf *)
                let _ = (qi --> QEPSILON) q in (* qi -{}---> q  *)
                ()
              end
              pl;
            qi
      in
      let qf = state () in
      let qi = loop qf p in
      let m = !count in

      (* Compute epsilon closure *)
      let graph = Array.make m IS.empty in
      List.iter
        begin fun (q,q') ->
          graph.(q) <- IS.add q' graph.(q)
        end
        !epsilons;

      let closure = Array.make m IS.empty in
      let rec transitive past = function
      | [] -> past
      | q :: future ->
          let past' = IS.add q past in
          let future' =
            IS.fold
              begin fun q' future' ->
                (* q -{}--> q' *)
                if IS.mem q' past' then
                  future'
                else
                  q' :: future'
              end
              graph.(q)
              future
          in
          transitive past' future'
      in
      for i = 0 to m - 1 do
        closure.(i) <- transitive IS.empty [i] (* O(n^2), I know *)
      done;

      (* Finally, build the table *)
      let table = Array.make m [] in
      List.iter
        begin fun (q,t,q') ->
          table.(q) <- (t, closure.(q')) :: table.(q)
        end
        !transitions;

      (graph, closure,
      { mc_qi = closure.(qi);
        mc_table = table;
        mc_qf = qf;
        mc_power_table = Hashtbl.create 37 })
    ;;
    let build x = let (_,_, machine) = build' x in machine;;
    (* ***)
    (*** run *)
    let run ?(trace=false) machine u =
      let m = String.length u in
      let apply qs c =
        try
          let t = Hashtbl.find machine.mc_power_table c in
          ISM.find qs t
        with
        | Not_found ->
            let qs' =
              IS.fold
                begin fun q qs' ->
                  List.fold_left
                    begin fun qs' (cl,qs'') ->
                      if match_character_class cl c then
                        IS.union qs' qs''
                      else
                        qs'
                    end
                    qs'
                    machine.mc_table.(q)
                end
                qs
                IS.empty
            in
            let t =
              try
                Hashtbl.find machine.mc_power_table c
              with
              | Not_found -> ISM.empty
            in
            Hashtbl.replace machine.mc_power_table c (ISM.add qs qs' t);
            qs'
      in
      let rec loop qs i =
        if IS.is_empty qs then
          false
        else
          begin
            if i = m then
              IS.mem machine.mc_qf qs
            else
              begin
                let c = u.[i] in
                if trace then
                  begin
                    Printf.printf "%d %C {" i c;
                    IS.iter (fun q -> Printf.printf " %d" q) qs;
                    Printf.printf " }\n%!"
                  end;
                let qs' = apply qs c in
                loop qs' (i + 1)
              end
          end
      in
      loop machine.mc_qi 0
    ;;
    (* ***)
  end
;;
(* ***)
(*** Brute *)
module Brute =
  struct
    exception Succeed;;
    exception Fail;;
    exception Too_hard;;

    (*** match_pattern *)
    let match_pattern counter p u =
      let m = String.length u in
      (** [loop i n p] returns [true] iff the word [u.(i .. i + n - 1)] is in the
       ** language generated by the pattern [p].
       ** We must have 0 <= i and i + n <= m *)
      let rec loop (i,n,p) =
        assert (0 <= i && 0 <= n && i + n <= m);
        incr counter;
        if !counter >= brute_limit then raise Too_hard;
        match p with
        | Word v   ->
            String.length v = n &&
            begin
              let rec check j = j = n || (v.[j] = u.[i + j] && check (j + 1))
              in
              check 0
            end
        | Epsilon  -> n = 0
        | Star(Class True) -> true
        | Star(Class cl) ->
            let rec check k =
              if k = n then
                true
              else
                (match_character_class cl u.[i + k]) && check (k + 1)
            in
            check 0
        | Star _ -> raise Too_hard
        | Class cl -> n = 1 && match_character_class cl u.[i]
        | Concat(p1,p2) ->
            let rec scan j =
              j <= n && ((loop (i,j,p1) && loop (i+j, n - j,p2)) || scan (j + 1))
            in
            scan 0
        | Union pl -> List.exists (fun p' -> loop (i,n,p')) pl
      in
      loop (0,m,p)
    ;;
    (* ***)
end
;;
(* ***)
(*** fast_pattern_contents, fast_pattern, globber *)
type fast_pattern_contents =
| Brute of int ref * pattern
| Machine of NFA.machine
;;
type fast_pattern = fast_pattern_contents ref;;
type globber = fast_pattern atom Bool.boolean;;
(* ***)
(*** fast_pattern_of_pattern *)
let fast_pattern_of_pattern p = ref (Brute(ref 0, p));;
(* ***)
(*** add_dir *)
let add_dir dir x =
  match dir with
  | None -> x
  | Some(dir) ->
      match x with
      | Constant(s) ->
          Constant(My_std.filename_concat dir s)
      | Pattern(p) ->
          Pattern(Concat(Word(My_std.filename_concat dir ""), p))
;;
(* ***)
(*** add_ast_dir *)
let add_ast_dir dir x =
  match dir with
  | None -> x
  | Some dir ->
      let slash = Class(Atom('/','/')) in
      let any = Class True in
      let q = Union[Epsilon; Concat(slash, Star any)] in (* ( /** )? *)
      And[Atom(Pattern(ref (Brute(ref 0, Concat(Word dir, q))))); x]
;;
(* ***)
(*** parse *)
let parse ?dir u =
  let l = Lexing.from_string u in
  let tok = ref None in
  let f =
    fun () ->
      match !tok with
      | None -> token l
      | Some x ->
          tok := None;
          x
  in
  let g t =
    match !tok with
    | None -> tok := Some t
    | Some t' ->
        raise (Parse_error(sf "Trying to unput token %s while %s is active" (string_of_token t) (string_of_token t')))
  in
  let read x =
    let y = f () in
    if x = y then
      ()
    else
      raise (Parse_error(sf "Unexpected token, expecting %s, got %s" (string_of_token x) (string_of_token y)))
  in
  let rec atomizer continuation = match f () with
  | NOT    -> atomizer (fun x -> continuation (Not x))
  | ATOM x ->
      begin
        let a =
          match add_dir dir x with
          | Constant u -> Constant u
          | Pattern p -> Pattern(fast_pattern_of_pattern p)
        in
        continuation (Atom a)
      end
  | TRUE   -> continuation True
  | FALSE  -> continuation False
  | LPAR   ->
      let y = parse_s () in
      read RPAR;
      continuation y
  | t      -> raise (Parse_error(sf "Unexpected token %s in atomizer" (string_of_token t)))
  and parse_s1 x = match f () with
  | OR     -> let y = parse_s () in Or[x; y]
  | AND    -> parse_t x
  | t      -> g t; x
  and parse_t1 x y = match f () with
  | OR     -> let z = parse_s () in Or[And[x;y]; z]
  | AND    -> parse_t (And[x;y])
  | t      -> g t; And[x;y]
  and parse_s () = atomizer parse_s1
  and parse_t x = atomizer (parse_t1 x)
  in
  let x = parse_s () in
  read EOF;
  add_ast_dir dir x
;;
(* ***)
(*** eval *)
let eval g u =
  Bool.eval
    begin function
      | Constant v -> u = v
      | Pattern kind ->
          match !kind with
          | Brute(count, p) ->
            begin
              let do_nfa () =
                let m = NFA.build p in
                kind := Machine m;
                NFA.run m u
              in
              if !count >= brute_limit then
                do_nfa ()
              else
                try
                  Brute.match_pattern count p u
                with
                | Brute.Too_hard -> do_nfa ()
            end
          | Machine m -> NFA.run m u
    end
    g
(* ***)
(*** Debug *)
(*let (Atom(Pattern x)) = parse "<{a,b}>";;
#install_printer IS.print;;
#install_printer ISM.print;;
let (graph, closure, machine) = build' x;;*)
(* ***)
