(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright (C) 2010 OCamlCore SARL                                   *)
(*                                                                     *)
(* See LICENSE for details.                                            *)
(***********************************************************************)

open Format

module type DIFF_ELEMENT = 
sig
  type t

  val pp_printer: Format.formatter -> t -> unit

  val compare: t -> t -> int

  val pp_print_sep: Format.formatter -> unit -> unit
end

module type S = 
sig
  type e 

  type t

  val compare: t -> t -> int

  val pp_printer: Format.formatter -> t -> unit

  val pp_diff: Format.formatter -> (t * t) -> unit

  val assert_equal: ?msg:string -> t -> t -> unit

  val of_list: e list -> t
end

let assert_equal ?msg compare pp_printer pp_diff exp act =
  OUnit.assert_equal 
    ~cmp:(fun t1 t2 -> (compare t1 t2) = 0)
    ~printer:(fun t -> 
                let buff = Buffer.create 13 in
                let fmt = formatter_of_buffer buff in
                  pp_printer fmt t; 
                  pp_print_flush fmt ();
                  Buffer.contents buff) 
    ~pp_diff 
    ?msg 
    exp act

module SetMake (D: DIFF_ELEMENT) : S with type e = D.t = 
struct 
  module Set = Set.Make(D)

  type e = D.t

  type t = Set.t

  let compare =
    Set.compare 

  let pp_printer fmt t = 
    let first = ref true in
      pp_open_box fmt 0;
      Set.iter 
        (fun e ->
           if not !first then
             D.pp_print_sep fmt ();
           D.pp_printer fmt e;
           first := false)
        t;
      pp_close_box fmt ()

  let pp_diff fmt (t1, t2) = 
    let first = ref true in
    let print_list c t = 
      Set.iter 
        (fun e ->
           if not !first then
             D.pp_print_sep fmt ();
           pp_print_char fmt c;
           D.pp_printer fmt e;
           first := false)
        t
    in
      pp_open_box fmt 0;
      print_list '+' (Set.diff t2 t1);
      print_list '-' (Set.diff t1 t2);
      pp_close_box fmt ()

  let assert_equal ?msg exp act =
    assert_equal ?msg compare pp_printer pp_diff exp act

  let of_list lst =
    List.fold_left
      (fun acc e ->
         Set.add e acc)
      Set.empty
      lst

end

module ListSimpleMake (D: DIFF_ELEMENT) : S with type e = D.t and type t = D.t list =
struct 
  type e = D.t

  type t = e list

  let rec compare t1 t2 = 
    match t1, t2 with
      | e1 :: tl1, e2 :: tl2 ->
          begin
            match D.compare e1 e2 with
              | 0 ->
                  compare tl1 tl2
              | n ->
                  n
          end

      | [], [] ->
          0

      | _, [] ->
          -1

      | [], _ ->
          1
  
  let pp_print_gen pre fmt t = 
    let first = ref true in
      pp_open_box fmt 0;
      List.iter 
        (fun e ->
           if not !first then 
             D.pp_print_sep fmt ();
           fprintf fmt "%s%a" pre D.pp_printer e;
           first := false)
        t;
      pp_close_box fmt ()

  let pp_printer fmt t = 
    pp_print_gen "" fmt t

  let pp_diff fmt (t1, t2) = 
    let rec pp_diff' n t1 t2 =
      match t1, t2 with
        | e1 :: tl1, e2 :: tl2 ->
            begin
              match D.compare e1 e2 with
                | 0 ->
                    pp_diff' (n + 1) tl1 tl2
                | _ ->
                    fprintf fmt
                      "element number %d differ (%a <> %a)"
                      n
                      D.pp_printer e1
                      D.pp_printer e2
            end

        | [], [] ->
            ()

        | [], lst ->
            fprintf fmt "at end,@ ";
            pp_print_gen "+" fmt lst

        | lst, [] ->
            fprintf fmt "at end,@ ";
            pp_print_gen "-" fmt lst
    in
      pp_open_box fmt 0;
      pp_diff' 0 t1 t2;
      pp_close_box fmt ()

  let assert_equal ?msg exp act =
    assert_equal ?msg compare pp_printer pp_diff exp act

  let of_list lst =
    lst
end

let pp_comma_separator fmt () =
  fprintf fmt ",@ "
