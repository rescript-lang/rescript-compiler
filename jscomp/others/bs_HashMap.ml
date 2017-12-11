(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(**  Adapted by Authors of BuckleScript 2017                           *)

(* For JS backends, we use [undefined] as default value, so that buckets
   could be allocated lazily
*)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b,'id) t0 =
  { mutable size: int;                        (* number of entries *)
    mutable buckets: ('a, 'b) bucketlist array;  (* the buckets *)
    initial_size: int;                        (* initial array size *)
  }
(* and 'a opt = 'a option *) (* TODO: conditionally compiled into native*)
#if BS then
and 'a opt = 'a Js.undefined
#else 
and 'a opt = 'a option 
#end
and ('a,'b) buckets =  
  
{
   mutable key : 'a ; 
   mutable value : 'b ; 
   mutable next : ('a, 'b) buckets opt
   }
and ('a,'b) bucketlist = ('a, 'b) buckets opt

type ('a,'b,'id) t = {
  dict : ('a, 'id) Bs_Hash.t;
  data : ('a,'b,'id) t0;

}
#if BS then
let toOpt = Js.Undefined.to_opt
let return = Js.Undefined.return
let emptyOpt = Js.undefined               
let makeSize s = Bs_Array.makeUninitialized s 
#else 
external toOpt : 'a -> 'a = "%identity"
let return x = Some x 
let emptyOpt = None
let makeSize s = Bs_Array.make s emptyOpt
#end

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}


let rec power_2_above x n =
  if x >= n then x
  else if x * 2 < x then x (* overflow *)
  else power_2_above (x * 2) n

let create0  initial_size =
  let s = power_2_above 16 initial_size in
  { initial_size = s; size = 0; 
    buckets = makeSize s  }

let clear0 h =
  h.size <- 0;
  let h_buckets = h.buckets in 
  let len = Array.length h_buckets in
  for i = 0 to len - 1 do
    Bs_Array.unsafe_set h_buckets i  emptyOpt
  done

let reset0 h =
  let len = Array.length h.buckets in
  let h_initial_size = h.initial_size in
  if len = h_initial_size then
    clear0 h
  else begin
    h.size <- 0;
    h.buckets <- makeSize h_initial_size 
  end

let length0 h = h.size


let rec do_bucket_iter ~f buckets = 
  match toOpt buckets with 
  | None ->
    ()
  | Some {key ; value; next } ->
    f key value [@bs]; do_bucket_iter ~f next

let iter0 f h =
  let d = h.buckets in
  for i = 0 to Array.length d - 1 do
    do_bucket_iter f (Bs_Array.unsafe_get d i)
  done


let rec do_bucket_fold ~f b accu =
  match toOpt b with
  | None ->
    accu
  | Some { key ;  value ; next } ->
    do_bucket_fold ~f next (f key value accu [@bs]) 

let fold0 f h init =
  let d = h.buckets in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket_fold ~f (Bs_Array.unsafe_get d i) !accu
  done;
  !accu



let rec bucket_length accu buckets = 
  match toOpt buckets with 
  | None -> accu
  | Some { next } -> bucket_length (accu + 1) next

let max (m : int) n = if m > n then m else n  

let logStats0 h =
  let mbl =
    Bs_Array.foldLeft (fun[@bs] m b -> max m (bucket_length 0 b)) 0 h.buckets in
  let histo = Bs_Array.make (mbl + 1) 0 in
  Bs_Array.iter
    (fun[@bs] b ->
       let l = bucket_length 0 b in
       Bs_Array.unsafe_set histo l (Bs_Array.unsafe_get histo l + 1)
    )
    h.buckets;
  Js.log [%obj{ num_bindings = h.size;
                num_buckets = Array.length h.buckets;
                max_bucket_length = mbl;
                bucket_histogram = histo }]


let rec filterMapInplaceBucket f h i prec buckets =
  match toOpt buckets with 
  | None ->
    begin match toOpt prec with
      | None -> Bs_Array.unsafe_set h.buckets i emptyOpt
      | Some c -> c.next <- emptyOpt
    end
  | (Some ({key; value = data} as c)) ->
    begin match f key data [@bs] with
      | None ->
        h.size <- h.size - 1; (* delete *)
        filterMapInplaceBucket f h i prec c.next
      | Some data -> (* replace *)
        begin match toOpt prec with
          | None -> Bs_Array.unsafe_set h.buckets i  buckets 
          | Some c -> c.next <- buckets
        end;
        c.value <- data;
        filterMapInplaceBucket f h i buckets c.next
    end

let filterMapInplace0 f h =
  let h_buckets = h.buckets in
  for i = 0 to Array.length h_buckets - 1 do
    filterMapInplaceBucket f h i emptyOpt (Bs_Array.unsafe_get h_buckets i)
  done

let key_index ~hash (h : ('a, _,_) t0) (key : 'a) =
  ((Bs_Hash.getHash hash) key [@bs]) land (Array.length h.buckets - 1)

let rec insert_bucket_list ~hash ~ndata h buckets = 
  match toOpt buckets with 
  | None -> ()
  | Some { key; value ; next} ->
    insert_bucket_list ~hash ~ndata h next; (* preserve original order of elements *)
    let nidx = key_index ~hash h key in
    Bs_Array.unsafe_set ndata nidx 
      ( return {key; value; next = Bs_Array.unsafe_get ndata nidx }) 

let rec insert_bucket ~hash ~ndata ~ndata_tail h buckets = 
  match toOpt buckets with 
  | None -> ()
  | Some ({key; next} as cell) ->
    let nidx = key_index ~hash h key in
    begin match toOpt (Bs_Array.unsafe_get ndata_tail nidx) with
      | None -> 
        let v = return cell in 
        Bs_Array.unsafe_set ndata_tail nidx  v;
        Bs_Array.unsafe_set ndata nidx  v

      | Some tail ->
        let v = return cell in 
        Bs_Array.unsafe_set ndata_tail nidx  v;
        tail.next <- v ; (* cell put at the end *)            
    end;          
    insert_bucket ~hash ~ndata ~ndata_tail h next


let resize ~hash h =
  let odata = h.buckets in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let ndata = makeSize nsize  in
    let ndata_tail = makeSize nsize  in (* keep track of tail *)
    h.buckets <- ndata;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      insert_bucket ~hash ~ndata ~ndata_tail h (Bs_Array.unsafe_get odata i)
    done;
    for i = 0 to nsize - 1 do
      match toOpt (Bs_Array.unsafe_get ndata_tail i) with
      | None -> ()
      | Some tail -> tail.next <- emptyOpt
    done
  end


let add0 ~hash h key info =
  let i = key_index ~hash h key in
  let h_buckets = h.buckets in  
  let bucket = {key; value = info; next= Bs_Array.unsafe_get h_buckets i} in  
  Bs_Array.unsafe_set h_buckets i  (return bucket);
  h.size <- h.size + 1;
  if h.size > Array.length h_buckets lsl 1 then resize ~hash  h


let rec remove_bucket ~eq h h_buckets  i key prec buckets =
  match toOpt buckets with
  | None -> ()
  | Some {key=k; next} ->
    if (Bs_Hash.getEq eq) k key [@bs]
    then 
      begin
        (match toOpt prec with
         | None -> Bs_Array.unsafe_set h_buckets i  next
         | Some c -> c.next <- next);
        h.size <- h.size - 1;        
      end
    else remove_bucket ~eq h h_buckets i key buckets next

let remove0 ~hash ~eq h key =
  let i = key_index ~hash h key in
  let h_buckets = h.buckets in 
  remove_bucket ~eq h h_buckets i key emptyOpt (Bs_Array.unsafe_get h_buckets i)

let rec removeAllBuckets ~eq h h_buckets  i key prec buckets =
  match toOpt buckets with
  | None -> ()
  | Some {key=k; next} ->
    if (Bs_Hash.getEq eq) k key [@bs]
    then 
      begin
        (match toOpt prec with
         | None -> Bs_Array.unsafe_set h_buckets i  next
         | Some c -> c.next <- next);
        h.size <- h.size - 1;        
      end;
    removeAllBuckets ~eq h h_buckets i key buckets next

let removeAll0 ~hash ~eq h key =
  let i = key_index ~hash h key in
  let h_buckets = h.buckets in 
  removeAllBuckets ~eq h h_buckets i key emptyOpt (Bs_Array.unsafe_get h_buckets i)


(* TODO: add [removeAll] *)


let rec find_rec ~eq key buckets = 
  match toOpt buckets with 
  | None ->
    None
  | Some { key = k; value = d; next =  rest} ->
    if (Bs_Hash.getEq eq) key k [@bs] then Some d else find_rec ~eq key  rest

let findOpt0 ~hash ~eq h key =
  match toOpt @@ Bs_Array.unsafe_get h.buckets (key_index ~hash h key) with
  | None -> None
  | Some {key = k1; value  = d1; next =  rest1} ->
    if (Bs_Hash.getEq eq) key k1 [@bs] then Some d1 else
      match toOpt rest1 with
      | None -> None
      | Some {key = k2; value =  d2; next =  rest2} ->
        if (Bs_Hash.getEq eq) key k2 [@bs] then Some d2 else
          match toOpt rest2 with
          | None -> None
          | Some { key = k3; value = d3; next =  rest3} ->
            if (Bs_Hash.getEq eq) key k3 [@bs] then Some d3 else find_rec ~eq key rest3


let findAll0 ~hash ~eq h key =
  let rec find_in_bucket buckets = 
    match toOpt buckets with 
    | None ->
      []
    | Some {key = k; value = d; next =  rest} ->
      if (Bs_Hash.getEq eq) k key [@bs]
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket (Bs_Array.unsafe_get h.buckets (key_index ~hash h key))

let rec replace_bucket ~eq  key info buckets = 
  match toOpt buckets with 
  | None ->
    true
  | Some ({key = k; value =  i; next} as slot) ->
    if (Bs_Hash.getEq eq) k key [@bs]
    then
      begin
        slot.key <- key;
        slot.value <- info;
        false
      end
    else
      replace_bucket ~eq key info next

let replace0 ~hash ~eq  h key info =
  let i = key_index ~hash h key in
  let h_buckets = h.buckets in 
  let l = Array.unsafe_get h_buckets i in  
  if replace_bucket ~eq key info l then begin
    Bs_Array.unsafe_set h_buckets i (return {key ; value = info; next = l});
    h.size <- h.size + 1;
    if h.size > Array.length h.buckets lsl 1 then resize ~hash h
  end 

let rec mem_in_bucket ~eq key buckets = 
  match toOpt buckets with 
  | None ->
    false
  | Some {key = k; value = d; next =  rest} ->
    (Bs_Hash.getEq eq) k key [@bs] || mem_in_bucket ~eq key rest     
let mem0 ~hash ~eq h key =
  mem_in_bucket ~eq key (Bs_Array.unsafe_get h.buckets (key_index ~hash h key))



(*  Wrapper  *)
let create dict initialize_size = 
  { data  = create0 initialize_size  ;
    dict }
let clear h = clear0 h.data
let reset h = reset0 h.data
let length h = length0 h.data                  
let iter f h = iter0 f h.data
let fold f h init = fold0 f h.data init 
let logStats h = logStats0 h.data

let add (type a) (type b ) (type id) (h : (a,b,id) t) (key:a) (info:b) = 
  let module M = (val  h.dict) in 
  add0 ~hash:M.hash h.data key info 

let remove (type a) (type b) (type id) (h : (a,b,id) t) (key : a) = 
  let module M = (val h.dict) in   
  remove0 ~hash:M.hash ~eq:M.eq h.data key 

let removeAll (type a) (type b) (type id) (h : (a,b,id) t) (key : a) = 
  let module M = (val h.dict) in   
  removeAll0 ~hash:M.hash ~eq:M.eq h.data key 
  
let findOpt (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let module M = (val h.dict) in   
  findOpt0 ~hash:M.hash ~eq:M.eq h.data key 

let findAll (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let module M = (val h.dict) in   
  findAll0 ~hash:M.hash ~eq:M.eq h.data key   

let replace (type a) (type b) (type id)  (h : (a,b,id) t) (key : a) (info : b) =
  let module M = (val h.dict) in 
  replace0 ~hash:M.hash ~eq:M.eq h.data key info

let mem (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let module M = (val h.dict) in   
  mem0 ~hash:M.hash ~eq:M.eq h.data key   

let filterMapInplace  f h =
  filterMapInplace0 f h.data
