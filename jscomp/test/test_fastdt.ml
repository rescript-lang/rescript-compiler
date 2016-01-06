open Array
open Printf
open Bigarray

module H = Hashtbl

module A = Array1

type int_array = (int, int_elt, c_layout) A.t

type float_pair = { c_t : float ; c_f : float }
type dt_node = { split : int ; if_t : dt ; if_f : dt }
and  dt = Node of dt_node | Leaf of float_pair  (* num true, num false *)

let mkfp a b = { c_t = a ; c_f = b }

let array_elem (a : int array) i =
  let _N = length a in
  let rec lin_search lo hi =  (* search *includes* both lo and hi *)
    if a.(lo) == i then true
    else if lo >= hi then false
    else if a.(lo) < i then lin_search (lo+1) hi
    else false in
  let rec bin_search lo hi = 
    if hi - lo < 5 then lin_search lo hi
    else 
      let mi = (lo+hi) / 2 in
        if mi >= _N then lin_search lo hi else
          let v  = a.(mi) in
            if v == i then true
            else if v < i then bin_search mi hi
            else bin_search lo mi in
    if _N == 0 then false else bin_search 0 (_N-1)

let split_white_re = Str.regexp "[ \t]+"
let split_white = Str.split split_white_re

let dict  = H.create 5
let dictN = ref 0
let get_fid str =
  try
    H.find dict str
  with Not_found -> (
    let n = !dictN in
      incr dictN;
      H.replace dict str n;
      n
  )

let clean_up_dict fcounts minfc =
  let okay_f = H.create 5 in
  let id = ref 0 in
    H.iter (fun str f ->
              if H.find fcounts f > minfc then (
                H.replace okay_f str !id;
                incr id;
              )
           ) dict;
    dictN := !id;
    H.clear dict;
    H.iter (H.replace dict) okay_f;
    ()

let rec map_filter f = function
    [] -> []
  | (x::xs) -> 
      let r = map_filter f xs in
        try (f x) :: r
        with _ -> r

let (/..) a b = if b == 0 then 0.5 else float_of_int a /. float_of_int b

let predict dt x =
  let rec predict' = function
      Leaf p -> p.c_t /. (p.c_t +. p.c_f)
    | Node n -> predict' (if array_elem x n.split then n.if_t else n.if_f) in
    predict' dt

let compute_tree_error tree =
  let rec cte acc = function
      Leaf p -> acc +. if p.c_t >= p.c_f then p.c_f else p.c_t
    | Node n -> cte (cte acc n.if_t) n.if_f in
    cte 0. tree


let predict_committee dts x =
  fold_right (fun (a,dt) z -> z +. a *. predict dt x) dts 0. /. fold_right (fun (a,_) z -> z+.a) dts 0.

let is_real_value f = match classify_float f with FP_infinite | FP_nan -> false | _ -> true

let find_split_feature c_t c_f _F _Y _W used validEx =
  let sqr x = x *. x in
  let plp2 x = if x <= 0. || x >= 1. then 0. else
    if x < 0.2 
    then 0.468995593589281168 +. 3.16992500144231215  *. (x -. 0.10) -. 8.01497244938313    *. sqr (x -. 0.10)
    else if x < 0.5
    then 0.934068055375491091 +. 0.893084796083488341 *. (x -. 0.35) -. 3.17075833162409548 *. sqr (x -. 0.35)
    else if x < 0.8
    then 0.934068055375491091 -. 0.893084796083488341 *. (x -. 0.65) -. 3.17075833162409548 *. sqr (x -. 0.65)
    else 0.934068055375491091 -. 0.893084796083488341 *. (x -. 0.90) -. 3.17075833162409548 *. sqr (x -. 0.90) in
  let best = ref None in
    for f = 0 to length _F - 1 do
      let _Ff = _F.(f) in
      if not (H.mem used f) then (
        let c_1t = ref 0. in
        let c_1f = ref 0. in
          for i = 0 to A.dim _Ff - 1 do
            let n = _Ff.{i} in
              if validEx.(n) > 0 then (
                if _Y.(n)
                then c_1t := !c_1t +. _W.(n)
                else c_1f := !c_1f +. _W.(n);
              );
          done;
(* compute c_t *)
          (* if !c_1t > c_t then failwith ("c_t=" ^ string_of_float c_t ^ " c_1t=" ^ string_of_float !c_1t);
             if !c_1f > c_f then failwith ("c_f=" ^ string_of_float c_f ^ " c_1f=" ^ string_of_float !c_1f ^ " N=" ^ string_of_int (length _Y) ^ " l=" ^ string_of_int (A.dim _Ff)); *)
        let c_0t = max 0. (c_t -. !c_1t) in
        let c_0f = max 0. (c_f -. !c_1f) in
        let sz1  = !c_1t +. !c_1f in
        let sz0  =  c_0t +.  c_0f in
          match !best with
              None ->
                let h    = (if sz1 > 0. then sz1 *. (plp2 (!c_1t /. sz1)) else 0.) +. 
                           (if sz0 > 0. then sz0 *. (plp2 ( c_0t /. sz0)) else 0.) in
                  best := Some (h, (c_0t,c_0f,!c_1t,!c_1f), f)
            | Some (h',_,_) ->
                if sz0 > sz1 then (
                  let h0 = (if sz0 > 0. then sz0 *. (plp2 ( c_0t /. sz0)) else 0.) in
                    if h0 < h' then 
                      let h = h0 +. (if sz1 > 0. then sz1 *. (plp2 (!c_1t /. sz1)) else 0.) in
                        if h < h' then 
                          best := Some (h, (c_0t,c_0f,!c_1t,!c_1f), f)
                ) else (
                  let h0 = (if sz1 > 0. then sz1 *. (plp2 (!c_1t /. sz1)) else 0.) in
                    if h0 < h' then 
                      let h = h0 +. (if sz0 > 0. then sz0 *. (plp2 ( c_0t /. sz0)) else 0.) in
                        if h < h' then 
                          best := Some (h, (c_0t,c_0f,!c_1t,!c_1f), f)
                )
      )
    done;
    !best


let trim_tree_same dt =
  let rec trim_tree_same' = function
      Node n ->
        let t = trim_tree_same' n.if_t in
        let f = trim_tree_same' n.if_f in
          (match t,f with
               Leaf p1, Leaf p2 ->
                 let tp = p1.c_t /. (p1.c_t +. p1.c_f) in
                 let fp = p2.c_t /. (p2.c_t +. p2.c_f) in
                   if abs_float (tp -. fp) < 1e-6
                   then Leaf (mkfp (p1.c_t +. p2.c_t) (p1.c_f +. p2.c_f))
                   else Node { split = n.split ; if_t = t; if_f = f }
             | _ -> Node { split = n.split ; if_t = t; if_f = f })
    | l -> l in
    trim_tree_same' dt

(* for normal building, validEx None *)
let build_dt (max_depth : int) (leaf_acc : float) (smooth : float) (validExO : int array option) (_F : int_array array) (_Y : bool array) (_W : float array) =
  eprintf "."; flush stderr;
  let _N = length _Y in
  let used = H.create 5 in

  let validEx = match validExO with None -> make _N 1 | Some a -> a in

  (* let myplus vex s = if vex > 0 then s+1 else s in *)

  let rec build_dt' depth c_t c_f =
    (* let dstr = String.make (depth*2) ' ' in *)
    let p_t = (smooth +. c_t) /. (c_t +. c_f +. 2. *. smooth) in
    let p_f = 1. -. p_t in
      if p_t < 0. || p_f < 0. then failwith "";
      if c_t <= 0. || c_f <= 0. || min p_t p_f < leaf_acc || depth >= max_depth then Leaf (mkfp c_t c_f) else (
        (* build the tree *)
        match find_split_feature c_t c_f _F _Y _W used validEx with
            None -> Leaf (mkfp c_t c_f)
          | Some (_,(c_0t,c_0f,c_1t,c_1f),f) ->
              let _Ff = _F.(f) in
              H.replace used f ();
              (* anything that has f set is not valid for right *)
              for m = 0 to A.dim _Ff - 1 do validEx.(_Ff.{m}) <- validEx.(_Ff.{m}) - 1; done;
              let r = build_dt' (depth+1) c_0t c_0f in

              (* now, anything that does have f set is valid for left *)
              for m = 0 to A.dim _Ff - 1 do validEx.(_Ff.{m}) <- validEx.(_Ff.{m}) + 2; done;
              for n = 0 to _N - 1 do validEx.(n) <- validEx.(n) - 1; done;
              let l = build_dt' (depth+1) c_1t c_1f in

              (* finally, return validEx to how it was before *)
              for m = 0 to A.dim _Ff - 1 do validEx.(_Ff.{m}) <- validEx.(_Ff.{m}) - 1; done;
              for n = 0 to _N - 1 do validEx.(n) <- validEx.(n) + 1; done;

              H.remove used f;

              Node { split = f ; if_t = l ; if_f = r }
      ) in

  let c_t = ref 0. in
  let c_f = ref 0. in
    for n = 0 to _N - 1 do
      if validEx.(n) > 0 then (
        if _Y.(n)
        then c_t := !c_t +. _W.(n)
        else c_f := !c_f +. _W.(n);
      )
    done;

  let t' = trim_tree_same (build_dt' 0 !c_t !c_f) in
  let treeerror = compute_tree_error t' in
    eprintf "%g..." treeerror; flush stderr;
    t'

(*
let transpose_data (_X : int array array) : int array array =
  let _N = length _X in
  let max_F = 1 + fold_right (fold_right max) _X 0 in
  let _F0 = init max_F (fun _ -> H.create 2) in
    iteri (fun n -> iter (fun f -> H.replace _F0.(f) n ())) _X;
    map (fun h -> 
           let a = make (H.fold (fun _ _ c -> c+1) h 0) 0 in
             ignore (H.fold (fun k _ i -> a.(i) <- k; i+1) h 0);
             fast_sort compare a;
             a
        ) _F0
*)

(*
let ex_X = 
 [|  [| 0; 1; 2; 3 |]  ;
     [| 0; 1; 4; 5 |]  ;
     [| 0; 1; 2; 6 |]  ;
     [| 0; 1; 4; 5 |]  ;
     [| 0; 1; 3; 7 |]  ;
     [| 0; 1; 4; 7 |]  |]
let ex_Y = [| true; false; true; false; true; false |]
let ex_W = map (fun _ -> 1.) ex_Y
let ex_F = transpose_data ex_X
*)

let build_bagged_dt (size : int) (max_depth : int) (leaf_acc : float) (smooth : float) _ (_F : int_array array) (_Y : bool array) (_W : float array) =
  let _N = length _Y in
  let validEx = make _N 1 in
  let new_W   = make _N 0. in
    init size (fun _ ->
                 (* let maxW = fold_right max _W 0. in *)
                   for n = 0 to _N - 1 do validEx.(n) <- 0; new_W.(n) <- 0.; done;
                   for n = 0 to _N - 1 do
                     let m = Random.int _N in
                       validEx.(m) <- 1;
                       new_W.(m) <- new_W.(m) +. _W.(m);
                   done;
                   (1., build_dt max_depth leaf_acc smooth (Some validEx) _F _Y new_W)
              )


let build_boosted_dt (size : int) (max_depth : int) (leaf_acc : float) (smooth : float) (input_f : string) (_F : int_array array) (_Y : bool array) (_W : float array) =
  let _N = length _Y in
  let validEx = make _N 1 in
  let new_W   = copy _W in
  let sum_W   = fold_right (+.) _W 0. in
  let dts     = make size (0., Leaf (mkfp 0. 0.)) in
  (* let logit y = log (y /. (1. -. y)) in *)
  let h = open_in input_f in
  let mrl () = try Some (input_line h) with End_of_file -> None in
    dts.(0) <- (1., build_dt max_depth leaf_acc smooth (Some validEx) _F _Y new_W);
    for i = 1 to size - 1 do
      seek_in h 0;
      let pred = make _N 0. in
      let rec read n =
        match mrl () with None -> () | Some l ->
          (match split_white l with
               (y::x) ->
                 let x = of_list (map_filter (H.find dict) x) in
                   fast_sort compare x;
                 let p = predict (snd dts.(i-1)) x in
                   pred.(n) <- p;
                   read (n+1)
             | [] -> read n) in
        read 0;
      let error = ref 0. in
        for n = 0 to _N - 1 do
          let p = pred.(n) >= 0.5 in
            if p != _Y.(n) then error := !error +. _W.(n);
        done;
        if !error > 0. then (
          let epsilon = !error /. sum_W in
          let alpha = 0.5 *. log ((1. -. epsilon) /. epsilon) in
            eprintf "[%g %g]" epsilon alpha;
          let sum  = ref 0. in
            for n = 0 to _N - 1 do
              let p = pred.(n) >= 0.5 in
              let v = if p == _Y.(n) then exp (0. -. alpha) else exp alpha in
                pred.(n) <- v;
                sum := !sum +. v;
            done;
            for n = 0 to _N - 1 do
              new_W.(n) <- new_W.(n) *. pred.(n) /. !sum;
            done;
            dts.(i) <- (alpha, build_dt max_depth leaf_acc smooth (Some validEx) _F _Y new_W);
        ) else (
          dts.(i) <- dts.(i-1);
        );
    done;
    dts

let build_single_dt (max_depth : int) (leaf_acc : float) (smooth : float) _ (_F : int_array array) (_Y : bool array) (_W : float array) =
  [|1., build_dt max_depth leaf_acc smooth None _F _Y _W |]

let uniq (l : int list) =
  let rec uniq' = function
      [] -> []
    | [x] -> [x]
    | (x::y::xs) ->
        if x == y then uniq' (y::xs) else x :: uniq' (y::xs) in
    uniq' l

let of_list_rev list =
  match list with
      [] -> [||]
    | (x::_) ->
        let n = List.length list in
        let a = make n x in
        let i = ref (n-1) in
          List.iter (fun z -> a.(!i) <- z; decr i) list;
          a

let load_data (minfc : int) (fp : string)  =
  let h  = open_in fp in
  let mrl () = try Some (input_line h) with End_of_file -> None in
  let _N = ref 0 in
  let fcount = H.create 10 in
  let add_count f = H.replace fcount f (1 + try H.find fcount f with Not_found -> 0) in
  let rec cnt () =
    match mrl () with None -> () | Some l ->
      (match split_white l with
           (y :: x) -> ( List.iter (fun z -> let f = get_fid z in add_count f) x; incr _N; cnt () )
         | [] -> cnt ()) in
    cnt ();
    if minfc > 0 then clean_up_dict fcount minfc;
  let _F = init !dictN (fun _ -> ref []) in
  let _Y = make !_N false in
  let _W = make !_N 1. in
    seek_in h 0;
  let rec read n =
    match mrl () with None -> () | Some l ->
      (match split_white l with
           (y :: x) -> 
             ( _Y.(n) <- float_of_string y > 0.5
             ; List.iter (fun z -> 
                            try 
                              let f = get_fid z in
                                if f < length _F then
                                  let a = _F.(get_fid z) in 
                                    a := n :: !a
                            with Not_found -> ()
                         ) x
             ; read (n+1) )
         | [] -> read n) in
    read 0;
    (map (fun l -> A.of_array int c_layout (of_list_rev (uniq !l))) _F, _Y, _W)

let predict_file model (fp : string) =
  let h = if fp = "-" then stdin else open_in fp in
  let sumW  = ref 0. in
  let error = ref 0. in
  let mrl () = try Some (input_line h) with End_of_file -> None in
  let rec read () =
    match mrl () with None -> () | Some l ->
      (match split_white l with
           (y :: x) ->
             let y = float_of_string y > 0.5 in
             let x = of_list (map_filter (H.find dict) x) in
               fast_sort compare x;
             let v = predict_committee model x in
             let p = v > 0.5 in
               if y != p then error := !error +. 1.;
               sumW := !sumW +. 1.;
               printf "%g\n" v;
               read ()
         | [] -> read ()) in
    read ();
    if fp <> "-" then close_in h;
    (!error, !sumW)
               

let print_tree out dt =
  let rdict = make !dictN "" in
    H.iter (fun str n -> rdict.(n) <- str) dict;

  let rec print_tree' = function
      Leaf p -> fprintf out "L %g %g\n" p.c_t p.c_f
    | Node n -> ( fprintf out "N %s\n" rdict.(n.split)
                ; print_tree' n.if_t
                ; print_tree' n.if_f ) in
    print_tree' dt

let rec read_tree h = 
  let l = input_line h in
    match split_white l with
        ["N" ; feat] ->
          let s = get_fid feat in
          let t = read_tree h in
          let f = read_tree h in
            Node { split = s ; if_f = f ; if_t = t }
      | ["L" ; nt ; nf] -> Leaf (mkfp (float_of_string nt) (float_of_string nf))
      | _ -> failwith ("malformed line: '" ^ l ^ "'")

let load_model fp =
  let h = open_in fp in
  let size = int_of_string (input_line h) in
  let model = make size (0., Leaf (mkfp 0. 0.)) in
    for i = 0 to size - 1 do
      let alpha = float_of_string (input_line h) in
      let tree  = read_tree h in
        model.(i) <- (alpha, tree);
    done;
    close_in h;
    model

let () =
  let boost_size = ref None in
  let bag_size   = ref None in
  let input_f    = ref None in
  let load_dt    = ref None in
  let max_d      = ref 10 in
  let rho        = ref 0. in
  let minfc      = ref 1 in
  let usage = "usage: FastDT [-boost #|-bag #] [-load dtfile] [-maxd (10)] [-rho (0)] [-minfc (1)] <data>\n" in
  let i  = ref 1 in
  let _I = length Sys.argv in
    while !i < _I do
      if      !i < _I-1 && Sys.argv.(!i) = "-boost" then ( boost_size := Some (int_of_string Sys.argv.(!i+1)) ; i := !i+2 )
      else if !i < _I-1 && Sys.argv.(!i) = "-bag"   then ( bag_size   := Some (int_of_string Sys.argv.(!i+1)) ; i := !i+2 )
      else if !i < _I-1 && Sys.argv.(!i) = "-load"  then ( load_dt    := Some (Sys.argv.(!i+1))               ; i := !i+2 )
      else if !i < _I-1 && Sys.argv.(!i) = "-maxd"  then ( max_d      := int_of_string   Sys.argv.(!i+1)      ; i := !i+2 )
      else if !i < _I-1 && Sys.argv.(!i) = "-rho"   then ( rho        := float_of_string Sys.argv.(!i+1)      ; i := !i+2 )
      else if !i < _I-1 && Sys.argv.(!i) = "-minfc" then ( minfc      := int_of_string   Sys.argv.(!i+1)      ; i := !i+2 )
      else if String.length Sys.argv.(!i) > 1 && String.get Sys.argv.(!i) 0 = '-' then failwith usage
      else ( input_f := Some Sys.argv.(!i); i := !i+1 )
    done;
  let input_f = match !input_f with None -> failwith usage | Some s -> s in
    (match !load_dt with
         None -> (
           let learn = 
             match !boost_size, !bag_size with
                 None  , None   -> build_single_dt 
               | Some n, None   -> build_boosted_dt n
               | None  , Some n -> build_bagged_dt n
               | _ -> failwith "cannot specify both -boost and -bag" in
           eprintf "Loading data from %s...\n" input_f; flush stderr;
           let (_F,_Y,_W) = load_data !minfc input_f in
           eprintf "%d examples, %d features\n" (length _Y) (length _F); flush stderr;
           eprintf "Building model...";
           let model = learn !max_d !rho 1e-6 input_f _F _Y _W in
             printf "%d\n" (length model);
             iter (fun (a,dt) -> printf "%g\n" a; print_tree stdout dt) model;
           eprintf "\n"; flush stderr;
         )
       | Some dt_file -> 
           eprintf "Loading model from %s...\n" dt_file; flush stderr;
           let model = load_model dt_file in
           eprintf "Predicting on %s...\n" input_f; flush stderr;
           let error,sum_W = predict_file model input_f in
           eprintf "Error = %g / %g = %g\n" error sum_W (error /. sum_W);
           ()
    );
    ()
