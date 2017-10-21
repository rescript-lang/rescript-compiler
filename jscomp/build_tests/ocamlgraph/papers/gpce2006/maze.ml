
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

let draw = false

module MakeMaze
  (G : Sig.G with type V.label = int * int) 
  (B : Builder.S with module G = G) =
struct

  module Visited = Set.Make(G.V)

  let make_maze n m =
    let nodes = 
      Array.init n (fun i -> Array.init m (fun j -> G.V.create (i,j)))
    in
    let g = 
      Array.fold_left (Array.fold_left (fun g v -> B.add_vertex g v))
	(B.empty ()) nodes
    in
    (* code from Jon Harrop *)
    let on_board x y = x >= 0 && x < n && y >= 0 && y < m in
    let random_neighbor visited v =
      let aux choices (i,j) =
	if on_board i j then
	  let cell = nodes.(i).(j) in
	  if not (Visited.mem cell visited) then cell :: choices else choices
	else
	  choices
      in    
      let x,y = G.V.label v in
      let choices = List.fold_left aux [] [x-1, y; x+1, y; x, y-1; x, y+1] in
      if choices = [] then 
	None 
      else
	Some (List.nth choices (Random.int (List.length choices)))
    in
    let rec make g visited stack cell =
      let visited = Visited.add cell visited in
      match random_neighbor visited cell with
	| Some neighbor ->
	    let g = B.add_edge g cell neighbor in
            make g visited (cell :: stack) neighbor
	| _ ->
            match stack with
	      | [] -> g
              | cell :: stack -> make g visited stack cell 
    in
    let g = make g Visited.empty [] nodes.(0).(0) in 
    eprintf "make_maze: V=%d E=%d@." (G.nb_vertex g) (G.nb_edges g);
    nodes, g

end

module Bench
  (G : Sig.G with type V.label = int * int) 
  (Maze : sig val make_maze : int -> int -> G.V.t array array * G.t end) =
struct

  module W = struct
    type label = G.E.label
    type t = int
    let weight _ = 1
    let zero = 0
    let add = (+)
    let compare = compare
  end
  module Dij = Path.Dijkstra(G)(W)

  module Dfs = Traverse.Dfs(G)

  module Bfs = Traverse.Bfs(G)
  
  let bench s = 
    Random.init 123;
    let dij = open_out (s ^ "_dij") in
    let fdij = formatter_of_out_channel dij in
    let dfs = open_out (s ^ "_dfs") in
    let fdfs = formatter_of_out_channel dfs in
    let bfs = open_out (s ^ "_bfs") in
    let fbfs = formatter_of_out_channel bfs in
    let mem = open_out (s ^ "_mem") in
    let fmem = formatter_of_out_channel mem in
    for i = 1 to 10 do
      eprintf "benchmarking %s i=%d/10@." s i;
      let n = 60 * i in
      let t_dij = ref 0.0 in
      let t_dfs = ref 0.0 in
      let t_bfs = ref 0.0 in
      for k = 1 to 5 do
	let nodes,g = Maze.make_maze n n in
	let src = nodes.(0).(0) in
	let dst = nodes.(n-1).(n-1) in
	let _,t = Time.utime (Dij.shortest_path g src) dst in
	t_dij := !t_dij +. t;
	let _,t = Time.utime (Dfs.prefix (fun v -> ())) g in
	t_dfs := !t_dfs +. t;
	let _,t = Time.utime (Bfs.iter (fun v -> ())) g in
	t_bfs := !t_bfs +. t
      done;
      fprintf fdij "%d %2.2f@." n (!t_dij /. 5.);
      fprintf fdfs "%d %2.2f@." n (!t_dfs /. 5.);
      fprintf fbfs "%d %2.2f@." n (!t_bfs /. 5.);
      let _,g = Maze.make_maze n n in
      fprintf fmem "%d %d@." n (Size.size_kb g);
    done

end

(***

  open Graphics

  let gstep = 10
  let w = n * gstep
  let h = m * gstep
  let () = if draw then open_graph (Printf.sprintf " %dx%d" w h)

  let draw_path p =
    set_color red;
    let draw_edge e =
      let sx,sy = G.V.label (G.E.src e) in
      let dx,dy = G.V.label (G.E.dst e) in
      moveto (sx * gstep + gstep/2) (sy * gstep + gstep/2);
      lineto (dx * gstep + gstep/2) (dy * gstep + gstep/2);
    in
    List.iter draw_edge p

  let draw_maze g = 
    clear_graph ();
    set_color black;
    for i = 0 to n-1 do
      for j = 0 to m-1 do
	let v = nodes.(i).(j) in
	let x = gstep * (i+1) in
	let y = gstep * (j+1) in
	if i < n-1 && not (G.mem_edge g v nodes.(i+1).(j)) then begin
	  moveto x y; lineto x (y-gstep)
	end;
	if j < m-1 && not (G.mem_edge g v nodes.(i).(j+1)) then begin
	  moveto x y; lineto (x-gstep) y
	end
      done
    done

  let min_path = 4 * max n m
  let max_path = 5 * max n m

  let () = 
    printf "n = %d m = %d min_path = %d max_path = %d@." 
      n m min_path max_path

  let add g v =
    let i,j = G.V.label v in
    match Random.int 4 with
      | 0 (*gauche*) when i > 0 -> G.add_edge g v nodes.(i-1).(j)
      | 1 (*droite*) when i < n-1 -> G.add_edge g v nodes.(i+1).(j)
      | 2 (*bas*) when j > 0 -> G.add_edge g v nodes.(i).(j-1)
      | 3 (*haut*) when j < m-1 -> G.add_edge g v nodes.(i).(j+1)
      | _ -> g
 
  let rec lab g0 g = 
    if draw then draw_maze g;
    let p,l = Time.print_utime (Dij.shortest_path g src) dst in
    eprintf "path length = %d@." l;
    if draw then begin
      draw_path p;
      if read_key () = 'q' then exit 0
    end;
    if l <= min_path then
      match g0 with
	| None -> assert false
	| Some g0 -> lab None g0
    else begin
      if l <= max_path then exit 0;
      let g1 = 
	List.fold_left 
	  (fun g e -> if Random.int 10 = 0 then add g (G.E.src e) else g)
	  g p
      in
      lab (Some g) g1
    end

  let () = lab None g
***)


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

(*
module IA = Imperative.Graph.Abstract(struct type t = int * int end)
module BIA = Bench(IA)(MakeMaze(IA)(Builder.I(IA)))
let () = BIA.bench "ia"

module PA = Persistent.Graph.Abstract(struct type t = int * int end)
module BPA = Bench(PA)(MakeMaze(PA)(Builder.P(PA)))
let () = BPA.bench "pa"

module Int = struct
  type t = int * int
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

module IC = Imperative.Graph.Concrete(Int)
module BIC = Bench(IC)(MakeMaze(IC)(Builder.I(IC)))
let () = BIC.bench "ic"

module PC = Persistent.Graph.Concrete(Int)
module BPC = Bench(PC)(MakeMaze(PC)(Builder.P(PC)))
let () = BPC.bench "pc"
*)

module IA = Imperative.Graph.Abstract(struct type t = int * int end)
module MazeIA = MakeMaze(IA)(Builder.I(IA))
let _ = MazeIA.make_maze 200 200

