(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type peg = Out | Empty | Peg

let board = [|
 [| Out; Out; Out; Out; Out; Out; Out; Out; Out|];
 [| Out; Out; Out; Peg; Peg; Peg; Out; Out; Out|];
 [| Out; Out; Out; Peg; Peg; Peg; Out; Out; Out|];
 [| Out; Peg; Peg; Peg; Peg; Peg; Peg; Peg; Out|];
 [| Out; Peg; Peg; Peg; Empty; Peg; Peg; Peg; Out|];
 [| Out; Peg; Peg; Peg; Peg; Peg; Peg; Peg; Out|];
 [| Out; Out; Out; Peg; Peg; Peg; Out; Out; Out|];
 [| Out; Out; Out; Peg; Peg; Peg; Out; Out; Out|];
 [| Out; Out; Out; Out; Out; Out; Out; Out; Out|]
|]


let print_peg = function
    Out -> print_string "."
  | Empty -> print_string " "
  | Peg -> print_string "$"


let print_board board =
 for i=0 to 8 do
   for j=0 to 8 do
    print_peg board.(i).(j)
   done;
   print_newline()
 done


type direction = { dx: int; dy: int }

let dir = [| {dx = 0; dy = 1}; {dx = 1; dy = 0};
             {dx = 0; dy = -1}; {dx = -1; dy = 0} |]

type move = { x1: int; y1: int; x2: int; y2: int }

let moves = Array.create 31 {x1=0;y1=0;x2=0;y2=0}

let counter = ref 0

exception Found

let rec solve m =
  counter := !counter + 1;
  if m = 31 then
    begin match board.(4).(4) with Peg -> true | _ -> false end
  else
    try
      if !counter mod 500 = 0 then begin
        print_int !counter; print_newline()
      end;
      for i=1 to 7 do
      for j=1 to 7 do
        match board.(i).(j) with
          Peg ->
            for k=0 to 3 do
              let d1 = dir.(k).dx in
              let d2 = dir.(k).dy in
              let i1 = i+d1 in
              let i2 = i1+d1 in
              let j1 = j+d2 in
              let j2 = j1+d2 in
              match board.(i1).(j1) with
                Peg ->
                  begin match board.(i2).(j2) with
                    Empty ->
(*
                      print_int i; print_string ", ";
                      print_int j; print_string ") dir ";
                      print_int k; print_string "\n";
*)
                      board.(i).(j) <- Empty;
                      board.(i1).(j1) <- Empty;
                      board.(i2).(j2) <- Peg;
                      if solve(m+1) then begin
                        moves.(m) <- { x1=i; y1=j; x2=i2; y2=j2 };
                        raise Found
                      end;
                      board.(i).(j) <- Peg;
                      board.(i1).(j1) <- Peg;
                      board.(i2).(j2) <- Empty
                    | _ -> ()
                  end
              | _ -> ()
            done
        | _ ->
            ()
      done
      done;
      false
    with Found ->
      true


let _ = if solve 0 then (print_string "\n"; print_board board)
