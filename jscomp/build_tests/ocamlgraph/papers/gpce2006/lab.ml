
open Graph

module Time = struct
  
  open Unix
    
  let utime f x =                                                   
    let u = (times()).tms_utime in                                  
    let y = f x in
    let ut = (times()).tms_utime -. u in
    (y,ut)
      
  let print_utime f x = 
    let (y,ut) = utime f x in
    Printf.printf "user time: %2.2f\n" ut;
    y
      
end

let n = 40
let m = 20
let min_path = n + m + (max n m / 2)
  
module LabP(G : Sig.P with type V.label = int*int) =
struct

  let nodes = 
    Array.init n (fun i -> Array.init m (fun j -> G.V.create (i,j)))
  let g = 
    Array.fold_left (Array.fold_left (fun g v -> G.add_vertex g v))
      G.empty nodes

  let src = nodes.(0).(0)
  let dst = nodes.(n-1).(m-1)

  module W = struct
    type label = G.E.label
    type t = int
    let weight _ = 1
    let zero = 0
    let add = (+)
    let compare = compare
  end
  module Dij = Path.Dijkstra(G)(W)

  let rec lab g = 
    try
      let p,l = Dij.shortest_path g src dst in
      if l >= min_path then 
	g
      else
	let g = 
	  List.fold_left 
	    (fun g e -> if Random.int 2 < 1 then G.remove_edge_e g e else g)
	    g p
	in
	lab g
    with Not_found ->
      let rec add g i =
	if i < min n m then
	  let i = Random.int n in
	  let j = Random.int m in
	  let v = nodes.(i).(j) in
	  let g = match Random.int 4 with
	    | 0 (*gauche*) when i > 0 -> G.add_edge g v nodes.(i-1).(j)
	    | 1 (*droite*) when i < n-1 -> G.add_edge g v nodes.(i+1).(j)
	    | 2 (*bas*) when j > 0 -> G.add_edge g v nodes.(i).(j-1)
	    | 3 (*haut*) when j < m-1 -> G.add_edge g v nodes.(i).(j+1)
	    | _ -> g
	  in
	  add g (i+1)
	else
	  g
      in
      lab (add g 0)

  let g = lab g

  open Graphics
  let w = 800
  let h = 600
  let () = open_graph (Printf.sprintf " %dx%d" w h)
  let sx = w / n
  let sy = h / m
  let () = 
    for i = 0 to n-1 do
      for j = 0 to m-1 do
	let v = nodes.(i).(j) in
	let x = sx * (i+1) in
	let y = sy * (j+1) in
	if i < n-1 && not (G.mem_edge g v nodes.(i+1).(j)) then begin
	  moveto x y; lineto x (y-sy)
	end;
	if j < m-1 && not (G.mem_edge g v nodes.(i).(j+1)) then begin
	  moveto x y; lineto (x-sx) y
	end
      done
    done;
    ignore (read_key ())      

(***
  module Gr = Graphviz.Neato
    (struct
       include G
       let graph_attributes _ = []
       let default_vertex_attributes _ = []
       let vertex_name v = 
	 let i,j = G.V.label v in string_of_int (i+j*m)
       let vertex_attributes _ = []
       let default_edge_attributes _ = []
       let edge_attributes _ = []
     end)

  let () = 
    let c = open_out "test.dot" in
    Gr.output_graph c g;
    close_out c;
    ignore (Sys.command "dot -Tps test.dot | gv --orientation=landscape -")
***)


end

module G = Persistent.Graph.Abstract(struct type t=int*int end)
module M = LabP(G)
