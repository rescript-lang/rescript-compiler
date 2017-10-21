
open Format
open Graph

(*let () = Random.self_init ()*)

module Time = struct
  
  open Unix
    
  let utime f x =                                                   
    let u = (times()).tms_utime in                                  
    let y = f x in
    let ut = (times()).tms_utime -. u in
    (y,ut)
      
  let print_utime f x = 
    let (y,ut) = utime f x in
    Format.printf "user time: %2.2f@." ut;
    y
      
end

module Make
  (G : Sig.G with type V.label = int)
  (C : Classic.S with type graph = G.t)
  (R : sig val destruct : G.t -> unit end) =
struct

  let bench s = 
    let c = open_out (s ^ "_c") in
    let fc = formatter_of_out_channel c in
    let d = open_out (s ^ "_d") in
    let fd = formatter_of_out_channel d in
    let m = open_out (s ^ "_mem_full") in
    let fm = formatter_of_out_channel m in
    for i = 1 to 10 do
      let v = 100 * i in
      let g,t = Time.utime C.full v in
      fprintf fc "%d %2.2f@." v t;
      fprintf fm "%d %d@." v (Size.size_kb g);
      let _,t = Time.utime R.destruct g in
      fprintf fd "%d %2.2f@." v t
    done;
    close_out c;
    close_out d;
    close_out m

end

module DestructP(G : Sig.P) = struct

  let destruct g = 
    let g = G.fold_edges_e (fun e g -> G.remove_edge_e g e) g g in
    let _ = G.fold_vertex (fun v g -> G.remove_vertex g v) g g in
    ()

end

module DestructI(G : Sig.I) = struct

  let destruct g = 
    G.iter_edges_e (fun e -> G.remove_edge_e g e) g;
    G.iter_vertex (fun v -> G.remove_vertex g v) g

end

module IA = Imperative.Graph.Abstract(struct type t = int end)
module BIA = Make(IA)(Classic.I(IA))(DestructI(IA))
let () = BIA.bench "ia"

module PA = Persistent.Graph.Abstract(struct type t = int end)
module BPA = Make(PA)(Classic.P(PA))(DestructP(PA))
let () = BPA.bench "pa"

module Int = struct
  type t = int
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

module IC = Imperative.Graph.Concrete(Int)
module BIC = Make(IC)(Classic.I(IC))(DestructI(IC))
let () = BIC.bench "ic"

module PC = Persistent.Graph.Concrete(Int)
module BPC = Make(PC)(Classic.P(PC))(DestructP(PC))
let () = BPC.bench "pc"

module M = Imperative.Matrix.Graph
module Mfull = struct
  type graph = M.t

  let fold_for i0 i1 f =
    let rec loop i v = if i > i1 then v else loop (i + 1) (f v i) in
    loop i0

  let full ?(self=true) n =
    let g = M.make n in
    for i = 0 to n-1 do
      for j = 0 to n-1 do
	if self || i <> j then M.add_edge g i j
      done
    done;
    g

  let divisors _ = assert false (*TODO*)
  let de_bruijn _ = assert false (*TODO*)
  let vertex_only _ = assert false (*TODO*)
end
module BM = Make(M)(Mfull)(DestructI(M))
let () = BM.bench "m"

