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
let return = Js_null.return 

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
  match Js.nullToOption (lastGet q )with
  | None ->
    lengthSet  q 1;
    firstSet q cell;
    lastSet q cell
  | Some last ->
    lengthSet q (lengthGet q + 1);
    nextSet last  cell;
    lastSet q  cell


  
let peek q =
  match Js.nullToOption (firstGet q ) with
  | None -> None
  | Some v -> Some (contentGet v)

let peekUndefined q =
  match Js.nullToOption (firstGet q ) with
  | None -> Js.undefined
  | Some v -> Js.Undefined.return (contentGet v)


let peekExn q =
  match Js.nullToOption (firstGet q ) with
  | None -> [%assert "Belt.Queue.Empty"]
  | Some v -> contentGet v

let pop q =
  match Js.nullToOption (firstGet q ) with
  | None -> None
  | Some x  ->
    let next = nextGet x in 
    if next = Js.null then 
      begin (* only one element*)
        clear q;
        Some (contentGet x)
      end
    else begin 
      lengthSet q (lengthGet q - 1);
      firstSet q next;
      Some(contentGet x) 
    end

let popExn q =
  match Js.nullToOption (firstGet q ) with
  | None -> [%assert "Empty"]
  | Some x  ->
    let next = nextGet x in 
    if next = Js.null then 
      begin (* only one element*)
        clear q;
        contentGet x
      end
    else begin 
      lengthSet q (lengthGet q - 1);
      firstSet q next;
      contentGet x 
    end    

let popUndefined q =
  match Js.nullToOption (firstGet q ) with
  | None -> Js.undefined
  | Some x  ->
    let next = nextGet x in 
    if next = Js.null then 
      begin (* only one element*)
        clear q;
        Js.Undefined.return (contentGet x)
      end
    else begin 
      lengthSet q (lengthGet q - 1);
      firstSet q next;
      Js.Undefined.return (contentGet x) 
    end

let rec copyAux qRes prev cell =
  match Js.nullToOption cell with
  | None -> lastSet qRes  prev; qRes
  | Some x  ->
    let content = contentGet x in 
    let res = return @@ node ~content ~next:null in
    begin match Js.nullToOption prev with
      | None -> firstSet qRes res
      | Some p -> nextSet p  res
    end;
    copyAux qRes res (nextGet x)

let copy q =
  copyAux (t  ~length:(lengthGet q) ~first:null ~last:null)  null (firstGet q)


      
let rec copyMapAux qRes prev cell f =
  match Js.nullToOption cell with
  | None -> lastSet qRes  prev; qRes
  | Some x  ->
    let content = f (contentGet x) [@bs] in 
    let res = return @@ node ~content ~next:null in
    begin match Js.nullToOption prev with (*TODO: optimize to remove such check*)
      | None -> firstSet qRes res
      | Some p -> nextSet p  res
    end;
    copyMapAux qRes res (nextGet x) f

let mapU q f =
  copyMapAux (t  ~length:(lengthGet q) ~first:null ~last:null)  null (firstGet q) f
    
let map q f = mapU q (fun [@bs] a -> f a)
    
let isEmpty q =
  lengthGet q = 0

let size q =
  lengthGet q

let rec iterAux cell f =
  match Js.nullToOption cell with
  | None -> ()
  | Some x  ->
    f (contentGet x) [@bs];
    iterAux (nextGet x) f

let forEachU q f =
  iterAux (firstGet q) f

let forEach q f = forEachU q (fun[@bs] a -> f a)
    
let rec foldAux f accu cell =
  match Js.nullToOption cell with
  | None -> accu
  | Some x  ->
    let accu = f accu (contentGet x) [@bs] in
    foldAux f accu (nextGet x)

let reduceU q  accu f =
  foldAux f accu (firstGet q)

let reduce q accu f = reduceU q accu (fun[@bs] a b -> f a b)
    
let transfer q1 q2 =
  if lengthGet q1 > 0 then
    match Js.nullToOption (lastGet q2) with
    | None ->
      lengthSet q2 (lengthGet q1);
      firstSet q2 (firstGet q1);
      lastSet q2 (lastGet q1);
      clear q1
    | Some l ->
      lengthSet q2  (lengthGet q2 + lengthGet q1);
      nextSet l (firstGet q1);
      lastSet q2 (lastGet  q1);
      clear q1

let rec fillAux i arr cell =       
  match Js.nullToOption cell with 
  | None -> ()
  | Some x ->
    A.setUnsafe arr i (contentGet x);
    fillAux (i + 1) arr (nextGet x) 

let toArray x =         
  let v = A.makeUninitializedUnsafe (lengthGet x) in 
  fillAux 0 v (firstGet x);
  v

(*TODO: optimize *)
let fromArray arr =
  let q = make () in
  for i = 0 to A.length arr - 1 do
    add q (A.getUnsafe arr i)
  done ;
  q

