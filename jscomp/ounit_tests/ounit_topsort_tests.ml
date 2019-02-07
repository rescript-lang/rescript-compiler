let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let handle graph = 
  let len = List.length graph in 
  let result = Ext_topsort.Edge_vec.make len in 
  List.iter (fun (id,deps) -> 
      Ext_topsort.Edge_vec.push result {id ; deps = Int_vec.of_list deps } 
    ) graph; 
  result 


let graph1 = 
  [ 
    0, [1;2];
    1, [2;3];
    2, [4];
    3, [];
    4, []
  ], [[0]; [1]; [2] ; [3;4]]


let graph2 = 
  [ 
    0, [1;2];
    1, [2;3];
    2, [4];
    3, [5];
    4, [5];
    5, []
  ],  
  [[0]; [1]; [2] ; [3;4]; [5]]

let graph3 = 
    [ 0,[1;2;3;4;5];
      1, [6;7;8] ;
      2, [6;7;8];
      3, [6;7;8];
      4, [6;7;8];
      5, [6;7;8];
      6, [];
      7, [] ;
      8, []
     ],
     [[0]; [1;2;3;4;5]; [6; 7; 8]]


let expect loc (graph1, v) = 
  let graph = handle graph1  in 
  let queue = Ext_topsort.layered_dfs graph  in 
  OUnit.assert_bool loc
    (Queue.fold (fun acc x -> Set_int.elements x::acc) [] queue =
     v)





let (=~) = OUnit.assert_equal
let suites = 
  __FILE__
  >:::
  [
    __LOC__ >:: begin fun _ -> 
      expect __LOC__ graph1;
      expect __LOC__ graph2 ;
      expect __LOC__ graph3
    end

  ]