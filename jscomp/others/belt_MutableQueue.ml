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
(* Adapted significantly by BuckleScript Authors                          *)
module A = Belt_Array

type 'a node = { 
  content: 'a; 
  mutable next: 'a cell 
}
and 'a cell = 'a node Js.null
and 'a t = {
  mutable length: int;
  mutable first: 'a cell;
  mutable last: 'a cell
} [@@bs.deriving abstract]

let null  = Js.null 
let return = Js.Null.return 

let make () = 
  t 
    ~length: 0
    ~first:null
    ~last:null


let clear q =
  lengthSet q  0;
  firstSet q  null;
  lastSet q  null

let add q x =
  let cell = return @@ node 
      ~content:x
      ~next:null in
  match Js.nullToOption (last q )with
  | None ->
    lengthSet  q 1;
    firstSet q cell;
    lastSet q cell
  | Some last ->
    lengthSet q (length q + 1);
    nextSet last  cell;
    lastSet q  cell


  
let peek q =
  match Js.nullToOption (first q ) with
  | None -> None
  | Some v -> Some (content v)

let peekUndefined q =
  match Js.nullToOption (first q ) with
  | None -> Js.undefined
  | Some v -> Js.Undefined.return (content v)


let peekExn q =
  match Js.nullToOption (first q ) with
  | None -> [%assert "Belt.Queue.Empty"]
  | Some v -> content v

let pop q =
  match Js.nullToOption (first q ) with
  | None -> None
  | Some x  ->
    let next = next x in 
    if next = Js.null then 
      begin (* only one element*)
        clear q;
        Some (content x)
      end
    else begin 
      lengthSet q (length q - 1);
      firstSet q next;
      Some(content x) 
    end

let popExn q =
  match Js.nullToOption (first q ) with
  | None -> [%assert "Empty"]
  | Some x  ->
    let next = next x in 
    if next = Js.null then 
      begin (* only one element*)
        clear q;
        content x
      end
    else begin 
      lengthSet q (length q - 1);
      firstSet q next;
      content x 
    end    

let popUndefined q =
  match Js.nullToOption (first q ) with
  | None -> Js.undefined
  | Some x  ->
    let next = next x in 
    if next = Js.null then 
      begin (* only one element*)
        clear q;
        Js.Undefined.return (content x)
      end
    else begin 
      lengthSet q (length q - 1);
      firstSet q next;
      Js.Undefined.return (content x) 
    end

let rec copyAux qRes prev cell =
  match Js.nullToOption cell with
  | None -> lastSet qRes  prev; qRes
  | Some x  ->
    let content = content x in 
    let res = return @@ node ~content ~next:null in
    begin match Js.nullToOption prev with
      | None -> firstSet qRes res
      | Some p -> nextSet p  res
    end;
    copyAux qRes res (next x)

let copy q =
  copyAux (t  ~length:(length q) ~first:null ~last:null)  null (first q)


      
let rec copyMapAux qRes prev cell f =
  match Js.nullToOption cell with
  | None -> lastSet qRes  prev; qRes
  | Some x  ->
    let content = f (content x) [@bs] in 
    let res = return @@ node ~content ~next:null in
    begin match Js.nullToOption prev with (*TODO: optimize to remove such check*)
      | None -> firstSet qRes res
      | Some p -> nextSet p  res
    end;
    copyMapAux qRes res (next x) f

let mapU q f =
  copyMapAux (t  ~length:(length q) ~first:null ~last:null)  null (first q) f
    
let map q f = mapU q (fun [@bs] a -> f a)
    
let isEmpty q =
  length q = 0

let size q =
  length q

let rec iterAux cell f =
  match Js.nullToOption cell with
  | None -> ()
  | Some x  ->
    f (content x) [@bs];
    iterAux (next x) f

let forEachU q f =
  iterAux (first q) f

let forEach q f = forEachU q (fun[@bs] a -> f a)
    
let rec foldAux f accu cell =
  match Js.nullToOption cell with
  | None -> accu
  | Some x  ->
    let accu = f accu (content x) [@bs] in
    foldAux f accu (next x)

let reduceU q  accu f =
  foldAux f accu (first q)

let reduce q accu f = reduceU q accu (fun[@bs] a b -> f a b)
    
let transfer q1 q2 =
  if length q1 > 0 then
    match Js.nullToOption (last q2) with
    | None ->
      lengthSet q2 (length q1);
      firstSet q2 (first q1);
      lastSet q2 (last q1);
      clear q1
    | Some l ->
      lengthSet q2  (length q2 + length q1);
      nextSet l (first q1);
      lastSet q2 (last  q1);
      clear q1

let rec fillAux i arr cell =       
  match Js.nullToOption cell with 
  | None -> ()
  | Some x ->
    A.setUnsafe arr i (content x);
    fillAux (i + 1) arr (next x) 

let toArray x =         
  let v = A.makeUninitializedUnsafe (length x) in 
  fillAux 0 v (first x);
  v

(*TODO: optimize *)
let fromArray arr =
  let q = make () in
  for i = 0 to A.length arr - 1 do
    add q (A.getUnsafe arr i)
  done ;
  q

