# 2 "hashset.cppo.ml"
type key = string
type seed = int
external caml_hash_mix_string : seed -> string -> seed  = "caml_hash_mix_string"
external final_mix : seed -> seed = "caml_hash_final_mix"
let hash (s : key) =   
  final_mix  (caml_hash_mix_string 0 s )
    

# 20 "hashset.cppo.ml"
module N = Belt_internalSetBuckets
module C = Belt_internalBucketsType
module A = Belt_Array

type t = (unit, unit, key) N.t 

let rec copyBucket  ~h_buckets ~ndata_tail  old_bucket = 
  match C.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = hash (N.key cell)  land (A.length h_buckets - 1) in 
    let v = C.return cell in 
    begin match C.toOpt (A.getUnsafe ndata_tail nidx) with
      | None -> 
        A.setUnsafe h_buckets nidx  v
      | Some tail ->
        N.nextSet tail v  (* cell put at the end *)            
    end;          
    A.setUnsafe ndata_tail nidx  v;
    copyBucket  ~h_buckets ~ndata_tail  (N.next cell)


let tryDoubleResize  h =
  let odata = C.buckets h in
  let osize = A.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    C.bucketsSet h  h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      copyBucket  ~h_buckets ~ndata_tail (A.getUnsafe odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (A.getUnsafe ndata_tail i) with
      | None -> ()
      | Some tail -> N.nextSet tail C.emptyOpt
    done
  end



let rec removeBucket  h h_buckets  i (key : key) prec cell =
  let cell_next = N.next cell in 
  if  (N.key cell) = key 
  then 
    begin
      N.nextSet prec cell_next;
      C.sizeSet h (C.size h - 1);        
    end
  else 
    match C.toOpt cell_next with 
    | None -> 
      ()
    | Some cell_next ->
      removeBucket h h_buckets i key cell cell_next

let remove h (key : key)=  
  let h_buckets = C.buckets h in 
  let i = hash key  land (A.length h_buckets - 1) in  
  let l = A.getUnsafe h_buckets i in 
  match C.toOpt l with 
  | None -> ()
  | Some cell -> 
    let next_cell = N.next cell in 
    if  (N.key cell) = key then 
      begin 
        C.sizeSet h (C.size h - 1) ;
        A.setUnsafe h_buckets i next_cell
      end
    else       
      match C.toOpt next_cell with 
      | None -> ()
      | Some next_cell -> 
        removeBucket h h_buckets i key cell next_cell


let rec addBucket  h (key : key)  cell = 
  if N.key cell <> key then
    let  n = N.next cell in 
    match C.toOpt n with 
    | None ->  
      C.sizeSet h (C.size h + 1);
      N.nextSet cell (C.return @@ N.bucket ~key ~next:C.emptyOpt);
    | Some n -> addBucket h  key  n

let add h (key : key)  =
  let h_buckets = C.buckets h in 
  let buckets_len = A.length h_buckets in 
  let i = hash key land (buckets_len - 1) in 
  let l = A.getUnsafe h_buckets i in  
  (match C.toOpt l with                                    
  | None -> 
    A.setUnsafe h_buckets i 
      (C.return @@ N.bucket ~key ~next:C.emptyOpt);
    C.sizeSet h (C.size  h + 1);  
  | Some cell -> 
    addBucket  h key cell);
  if C.size h > buckets_len lsl 1 then tryDoubleResize  h

let rec memInBucket (key : key) cell = 
  N.key cell = key  || 
  (match C.toOpt (N.next cell) with 
   | None -> false 
   | Some nextCell -> 
     memInBucket key nextCell)

let has h key =
  let h_buckets = C.buckets h in 
  let nid = hash key  land (A.length h_buckets - 1) in 
  let bucket = A.getUnsafe h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    memInBucket key bucket


let make ~hintSize = C.make ~hintSize ~hash:() ~eq:()

let clear = C.clear
let size = C.size
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

