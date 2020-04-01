# 2 "others/hashset.cppo.ml"
type key = string
type seed = int
external caml_hash_mix_string : seed -> string -> seed  = "caml_hash_mix_string"
external final_mix : seed -> seed = "caml_hash_final_mix"
let hash (s : key) =   
  final_mix  (caml_hash_mix_string 0 s )
    

# 20 "others/hashset.cppo.ml"
module N = Belt_internalSetBuckets
module C = Belt_internalBucketsType
module A = Belt_Array

type t = (unit, unit, key) N.t 

let rec copyBucket  ~h_buckets ~ndata_tail  old_bucket = 
  match C.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = hash cell.N.key  land (A.length h_buckets - 1) in 
    let v = C.return cell in 
    begin match C.toOpt (A.getUnsafe ndata_tail nidx) with
      | None -> 
        A.setUnsafe h_buckets nidx  v
      | Some tail ->
        tail.N.next <- v  (* cell put at the end *)            
    end;          
    A.setUnsafe ndata_tail nidx  v;
    copyBucket  ~h_buckets ~ndata_tail  cell.N.next


let tryDoubleResize  h =
  let odata = h.C.buckets  in
  let osize = A.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    h.C.buckets <-h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      copyBucket  ~h_buckets ~ndata_tail (A.getUnsafe odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (A.getUnsafe ndata_tail i) with
      | None -> ()
      | Some tail -> tail.N.next <- C.emptyOpt
    done
  end



let rec removeBucket  h h_buckets  i (key : key) prec cell =
  let cell_next = cell.N.next in 
  if  cell.N.key = key 
  then 
    begin
      prec.N.next <- cell_next;
      h.C.size <- (h.C.size - 1);        
    end
  else 
    match C.toOpt cell_next with 
    | None -> 
      ()
    | Some cell_next ->
      removeBucket h h_buckets i key cell cell_next

let remove h (key : key)=  
  let h_buckets = h.C.buckets  in 
  let i = hash key  land (A.length h_buckets - 1) in  
  let l = A.getUnsafe h_buckets i in 
  match C.toOpt l with 
  | None -> ()
  | Some cell -> 
    let next_cell = cell.N.next in 
    if  cell.N.key = key then 
      begin 
        h.C.size <- (h.C.size- 1) ;
        A.setUnsafe h_buckets i next_cell
      end
    else       
      match C.toOpt next_cell with 
      | None -> ()
      | Some next_cell -> 
        removeBucket h h_buckets i key cell next_cell


let rec addBucket  h (key : key)  cell = 
  if cell.N.key <> key then
    let  n = cell.N.next in 
    match C.toOpt n with 
    | None ->  
      h.C.size <- (h.C.size+ 1);
      cell.N.next <- (C.return {N.key;  next = C.emptyOpt});
    | Some n -> addBucket h  key  n

let add h (key : key)  =
  let h_buckets = h.C.buckets  in 
  let buckets_len = A.length h_buckets in 
  let i = hash key land (buckets_len - 1) in 
  let l = A.getUnsafe h_buckets i in  
  (match C.toOpt l with                                    
  | None -> 
    A.setUnsafe h_buckets i 
      (C.return {N.key ; next = C.emptyOpt});
    h.C.size <- (h.C.size + 1);  
  | Some cell -> 
    addBucket  h key cell);
  if h.C.size> buckets_len lsl 1 then tryDoubleResize  h

let rec memInBucket (key : key) cell = 
  cell.N.key = key  || 
  (match C.toOpt cell.N.next with 
   | None -> false 
   | Some nextCell -> 
     memInBucket key nextCell)

let has h key =
  let h_buckets = h.C.buckets  in 
  let nid = hash key  land (A.length h_buckets - 1) in 
  let bucket = A.getUnsafe h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    memInBucket key bucket


let make ~hintSize = C.make ~hintSize ~hash:() ~eq:()

let clear = C.clear
let size h = h.C.size
let forEachU = N.forEachU
let forEach = N.forEach 
let reduceU = N.reduceU
let reduce = N.reduce
let logStats = N.logStats
let toArray = N.toArray
let copy = N.copy
let getBucketHistogram = N.getBucketHistogram 
let isEmpty = C.isEmpty

let fromArray arr = 
  let len = A.length arr in 
  let v = C.make ~hintSize:len ~hash:() ~eq:() in 
  for i = 0 to len - 1 do 
    add v (A.getUnsafe arr i)
  done ;
  v

(* TOOD: optimize heuristics for resizing *)  
let mergeMany h arr =   
  let len = A.length arr in 
  for i = 0 to len - 1 do 
    add h (A.getUnsafe arr i)
  done

