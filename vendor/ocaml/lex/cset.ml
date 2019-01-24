(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Luc Maranget, Jerome Vouillon projet Cristal,              *)
(*                          INRIA Rocquencourt                            *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

exception Bad

type t = (int * int) list


let empty = []
let is_empty = function
  | [] -> true
  | _  -> false

let singleton c = [c,c]

let interval c1 c2 =
  if c1 <= c2 then [c1,c2]
  else [c2,c1]


let rec union s1 s2 = match s1,s2 with
| [],_ -> s2
| _,[] -> s1
| (c1,d1) as p1::r1, (c2,d2)::r2 ->
    if c1 > c2 then
      union s2 s1
    else begin (* c1 <= c2 *)
      if d1+1 < c2 then
        p1::union r1 s2
      else if d1 < d2 then
        union ((c1,d2)::r2) r1
      else
        union s1 r2
    end

let rec inter l l' =  match l, l' with
    _, [] -> []
  | [], _ -> []
  | (c1, c2)::r, (c1', c2')::r' ->
      if c2 < c1' then
        inter r l'
      else if c2' < c1 then
        inter l r'
      else if c2 < c2' then
        (max c1 c1', c2)::inter r l'
      else
        (max c1 c1', c2')::inter l r'

let rec diff l l' =  match l, l' with
    _, [] -> l
  | [], _ -> []
  | (c1, c2)::r, (c1', c2')::r' ->
      if c2 < c1' then
        (c1, c2)::diff r l'
      else if c2' < c1 then
        diff l r'
      else
        let r'' = if c2' < c2 then (c2' + 1, c2) :: r else r in
        if c1 < c1' then
          (c1, c1' - 1)::diff r'' r'
        else
          diff r'' r'


let eof = singleton 256
and all_chars = interval 0 255
and all_chars_eof = interval 0 256

let complement s = diff all_chars s

let env_to_array env = match env with
| []         -> assert false
| (_,x)::rem ->
    let res = Array.make 257 x in
    List.iter
      (fun (c,y) ->
        List.iter
          (fun (i,j) ->
            for k=i to j do
              res.(k) <- y
            done)
          c)
      rem ;
    res
