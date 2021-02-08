(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Francois Pottier, projet Cristal, INRIA Rocquencourt           *)
(*                  Jeremie Dimino, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* Adapted significantly by ReScript Authors                          *)
module A = Belt_Array

type 'a node = { 
  content: 'a; 
  mutable next: 'a cell 
}
and 'a cell = 'a node option 
and 'a t = {
  mutable length: int;
  mutable first: 'a cell;
  mutable last: 'a cell
} 




let make () =   
    {
      length = 0;
      first = None;
      last = None }


let clear q =
  q.length <-  0;
  q.first <-  None;
  q.last <-  None

let add q x =
  let cell = Some {
      content = x;
      next = None } in
  match  q.last with
  | None -> (* TODO: better names for intermediate var *)
    q.length <- 1;
    q.first <- cell;
    q.last <- cell
  | Some last ->
    q.length <- q.length + 1;
    last.next <-  cell;
    q.last <-  cell


  
let peek q =
  match  q.first with (* same here could be v *)
  | None -> None
  | Some v -> Some v.content

let peekUndefined q =
  match  q.first with
  | None -> Js.undefined
  | Some v -> Js.Undefined.return v.content


let peekExn q =
  match  q.first with
  | None -> raise Not_found
  | Some v -> v.content

let pop q =
  match  q.first with
  | None -> None
  | Some x  ->
    let next = x.next in 
    if next = None then 
      begin (* only one element*)
        clear q;
        Some x.content
      end
    else begin 
      q.length <- q.length  - 1;
      q.first <- next;
      Some x.content 
    end

let popExn q = (* TO fix *)
  match  q.first with 
  | None -> raise Not_found
  | Some x  ->
    let next = x.next in 
    if next = None then 
      begin (* only one element*)
        clear q;
        x.content
      end
    else begin 
      q.length <- q.length - 1;
      q.first <- next;
      x.content 
    end    

let popUndefined q =
  match  q.first with
  | None -> Js.undefined
  | Some x  ->
    let next = x.next in 
    if next = None then 
      begin (* only one element*)
        clear q;
        Js.Undefined.return x.content
      end
    else begin 
      q.length <- q.length - 1;
      q.first <- next;
      Js.Undefined.return x.content 
    end

let rec copyAux qRes prev cell =
  match  cell with
  | None -> qRes.last <-  prev; qRes
  | Some x  ->
    let content = x.content in 
    let res =  Some {content ; next = None}  in
    begin match  prev with
      | None ->  qRes.first <- res
      | Some p -> p.next <-  res
    end;
    copyAux qRes res (x.next)

let copy q =
  copyAux {length = q.length; first = None;  last = None}  None q.first


      
let rec copyMapAux qRes prev cell f =
  match  cell with
  | None ->  qRes.last <- prev; qRes
  | Some x  ->
    let content = f x.content [@bs] in 
    let res = Some {content; next = None}  in
    begin match  prev with (*TODO: optimize to remove such check*)
      | None -> qRes.first <- res
      | Some p -> p.next <-  res
    end;
    copyMapAux qRes res x.next f

let mapU q f =
  copyMapAux {length = q.length; first = None; last = None}  None q.first f
    
let map q f = mapU q (fun [@bs] a -> f a)
    
let isEmpty q =
  q.length = 0

let size q =
  q.length

let rec iterAux cell f =
  match  cell with
  | None -> ()
  | Some x  ->
    f x.content [@bs];
    iterAux (x.next) f

let forEachU q f =
  iterAux q.first f

let forEach q f = forEachU q (fun[@bs] a -> f a)
    
let rec foldAux f accu cell =
  match  cell with
  | None -> accu
  | Some x  ->
    let accu = f accu x.content [@bs] in
    foldAux f accu (x.next)

let reduceU q  accu f =
  foldAux f accu q.first

let reduce q accu f = reduceU q accu (fun[@bs] a b -> f a b)
    
let transfer q1 q2 =
  if q1.length > 0 then
    match  q2.last with
    | None ->
      q2.length <- q1.length;
      q2.first <- q1.first;
      q2.last <- q1.last;
      clear q1
    | Some l ->
      q2.length <- (q2.length + q1.length);
      l.next <- q1.first;
      q2.last <- q1.last;
      clear q1

let rec fillAux i arr cell =       
  match  cell with 
  | None -> ()
  | Some x ->
    A.setUnsafe arr i x.content;
    fillAux (i + 1) arr (x.next) 

let toArray x =         
  let v = A.makeUninitializedUnsafe x.length in 
  fillAux 0 v x.first;
  v

(*TODO: optimize *)
let fromArray arr =
  let q = make () in
  for i = 0 to A.length arr - 1 do
    add q (A.getUnsafe arr i)
  done ;
  q

