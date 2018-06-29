
let suites = Mt.[
    __LOC__ , (fun _ -> 
        Eq ([|2;4|], 
            let  x = [|1;2;3;4;5|] in 
            Js.Vector.filterInPlace (fun[@bs] x ->  x mod 2 = 0) x;
            x

           )
      );
    __LOC__, (fun _ -> 
        Eq ( true, 
             let  x= [|1;2;3;4;5|] in 
             Js.Vector.filterInPlace  (fun [@bs] x -> x > 10) x ;
             Array.length x = 0
           )
      );
    (* es2015, unable to test because nothing currently implements array_like
    "from", (fun _ ->
      Eq(
        [| 0; 1 |],
        [| "a"; "b" |] |> Js.Array.keys |> Js.Array.from)
    );
    *)

    (* es2015, unable to test because nothing currently implements array_like
    "fromMap", (fun _ ->
      Eq(
        [| (-1); 0 |],
        Js.Array.fromMap
          ([| "a"; "b" |] |> Js.Array.keys)
          ((fun x -> x - 1) [@bs]))
    );
    *)

    (* es2015 *)
    "isArray_array", (fun _ ->
      Eq(true, [||] |> Js.Array.isArray)
    );
    "isArray_int", (fun _ ->
      Eq(false, 34 |> Js.Array.isArray)
    );

    "length", (fun _ ->
      Eq(3, [| 1; 2; 3 |] |> Js.Array.length)
    );

    (* es2015 *)
    "copyWithin", (fun _ ->
      Eq([| 1; 2; 3; 1; 2 |],
         [| 1; 2; 3; 4; 5 |] |> Js.Array.copyWithin ~to_:(-2))
    );
    "copyWithinFrom", (fun _ ->
      Eq([| 4; 5; 3; 4; 5 |],
         [| 1; 2; 3; 4; 5 |] |> Js.Array.copyWithinFrom ~to_:0 ~from:3)
    );
    "copyWithinFromRange", (fun _ ->
      Eq([| 4; 2; 3; 4; 5 |],
         [| 1; 2; 3; 4; 5 |] |> Js.Array.copyWithinFromRange ~to_:0 ~start:3 ~end_:4)
    );

    (* es2015 *)
    "fillInPlace", (fun _ ->
      Eq([| 4; 4; 4 |],
         [| 1; 2; 3 |] |> Js.Array.fillInPlace 4)
    );
    "fillFromInPlace", (fun _ ->
      Eq([| 1; 4; 4 |],
         [| 1; 2; 3 |] |> Js.Array.fillFromInPlace 4 ~from:1)
    );
    "fillRangeInPlace", (fun _ ->
      Eq([| 1; 4; 3 |],
         [| 1; 2; 3 |] |> Js.Array.fillRangeInPlace 4 ~start:1 ~end_:2)
    );

    "pop", (fun _ ->
      Eq(Some 3, [| 1; 2; 3 |] |> Js.Array.pop)
    );
    "pop - empty array", (fun _ ->
      Eq(None, [||] |> Js.Array.pop)
    );
    "push", (fun _ ->
      Eq(4, [| 1; 2; 3 |] |> Js.Array.push 4)
    );
    "pushMany", (fun _ ->
      Eq(5, [| 1; 2; 3 |] |> Js.Array.pushMany [| 4; 5 |])
    );

    "reverseInPlace", (fun _ ->
      Eq([| 3; 2; 1 |],
         [| 1; 2; 3 |] |> Js.Array.reverseInPlace)
    );

    "shift", (fun _ ->
      Eq(Some 1, [| 1; 2; 3 |] |> Js.Array.shift)
    );
    "shift - empty array", (fun _ ->
      Eq(None, [||] |> Js.Array.shift)
    );

    "sortInPlace", (fun _ ->
      Eq([| 1; 2; 3 |],
         [| 3; 1; 2 |] |> Js.Array.sortInPlace)
    );
    "sortInPlaceWith", (fun _ ->
      Eq([| 3; 2; 1 |],
         [| 3; 1; 2 |] |> Js.Array.sortInPlaceWith ((fun a b -> b - a) ))
    );

    "spliceInPlace", (fun _ ->
      let arr = [| 1; 2; 3; 4 |] in
      let removed = arr |> Js.Array.spliceInPlace ~pos:2 ~remove:0 ~add:[| 5 |] in

      Eq(([| 1; 2; 5; 3; 4 |], [||]), (arr, removed))
    );
    "removeFromInPlace", (fun _ ->
      let arr = [| 1; 2; 3; 4 |] in
      let removed = arr |> Js.Array.removeFromInPlace ~pos:2 in

      Eq(([| 1; 2 |], [| 3; 4 |]), (arr, removed))
    );
    "removeCountInPlace", (fun _ ->
      let arr = [| 1; 2; 3; 4 |] in
      let removed = arr |> Js.Array.removeCountInPlace ~pos:2 ~count:1 in

      Eq(([| 1; 2; 4 |], [| 3 |]), (arr, removed))
    );

    "unshift", (fun _ ->
      Eq(4, [| 1; 2; 3 |] |> Js.Array.unshift 4)
    );
    "unshiftMany", (fun _ ->
      Eq(5, [| 1; 2; 3 |] |> Js.Array.unshiftMany [| 4; 5 |])
    );

    "append", (fun _ ->
      Eq([| 1; 2; 3; 4 |],
         [| 1; 2; 3 |] |> Js.Array.concat [|4|])
    );
    "concat", (fun _ ->
      Eq([| 1; 2; 3; 4; 5 |],
         [| 1; 2; 3 |] |> Js.Array.concat [| 4; 5 |])
    );
    "concatMany", (fun _ ->
      Eq([| 1; 2; 3; 4; 5; 6; 7 |],
         [| 1; 2; 3 |] |> Js.Array.concatMany [| [| 4; 5; |]; [| 6; 7; |] |])
    );

    (* es2016 *)
    "includes", (fun _ ->
      Eq(true, [| 1; 2; 3 |] |> Js.Array.includes 3)
    );

    "indexOf", (fun _ ->
      Eq(1, [| 1; 2; 3 |] |> Js.Array.indexOf 2)
    );
    "indexOfFrom", (fun _ ->
      Eq(3, [| 1; 2; 3; 2 |] |> Js.Array.indexOfFrom 2 ~from:2)
    );

    "join", (fun _ ->
      Eq("1,2,3", [| 1; 2; 3 |] |> Js.Array.join)
    );
    "joinWith", (fun _ ->
      Eq("1;2;3", [| 1; 2; 3 |] |> Js.Array.joinWith ";")
    );

    "lastIndexOf", (fun _ ->
      Eq(1, [| 1; 2; 3 |] |> Js.Array.lastIndexOf 2)
    );
    "lastIndexOfFrom", (fun _ ->
      Eq(1, [| 1; 2; 3; 2 |] |> Js.Array.lastIndexOfFrom 2 ~from:2)
    );

    "slice", (fun _ ->
      Eq([| 2; 3; |],
         [| 1; 2; 3; 4; 5 |] |> Js.Array.slice ~start:1 ~end_:3)
    );
    "copy", (fun _ ->
      Eq([| 1; 2; 3; 4; 5 |],
         [| 1; 2; 3; 4; 5 |] |> Js.Array.copy)
    );
    "sliceFrom", (fun _ ->
      Eq([| 3; 4; 5 |],
         [| 1; 2; 3; 4; 5 |] |> Js.Array.sliceFrom 2)
    );

    "toString", (fun _ ->
      Eq("1,2,3", [| 1; 2; 3 |] |> Js.Array.toString)
    );
    "toLocaleString", (fun _ ->
      Eq("1,2,3", [| 1; 2; 3 |] |> Js.Array.toLocaleString)
    );

    (* es2015, iterator
    "entries", (fun _ ->
      Eq([| (0, "a"); (1, "b"); (2, "c") |],
         [| "a"; "b"; "c" |] |> Js.Array.entries |> Js.Array.from)
    );
    *)

    "every", (fun _ ->
      Eq(true, [| 1; 2; 3 |] |> Js.Array.every ((fun n ->  (n > 0)) ))
    );
    "everyi", (fun _ ->
      Eq(false, [| 1; 2; 3 |] |> Js.Array.everyi ((fun _ i ->  (i > 0)) ))
    );

    "filter", (fun _ ->
      Eq([| 2; 4 |],
         [| 1; 2; 3; 4 |] |> Js.Array.filter ((fun n -> n mod 2 = 0) ))
    );
    "filteri", (fun _ ->
      Eq([| 1; 3 |],
         [| 1; 2; 3; 4 |] |> Js.Array.filteri ((fun _ i ->  (i mod 2 = 0)) ))
    );

    (* es2015 *)
    "find", (fun _ ->
      Eq(Some 2, [| 1; 2; 3; 4 |] |> Js.Array.find ((fun n -> n mod 2 = 0) ))
    );
    "find - no match", (fun _ ->
      Eq(None, [| 1; 2; 3; 4 |] |> Js.Array.find ((fun n -> n mod 2 = 5) ))
    );
    "findi", (fun _ ->
      Eq(Some 1, [| 1; 2; 3; 4 |] |> Js.Array.findi ((fun _ i -> i mod 2 = 0) ))
    );
    "findi - no match", (fun _ ->
      Eq(None, [| 1; 2; 3; 4 |] |> Js.Array.findi ((fun _ i -> i mod 2 = 5) ))
    );

    (* es2015 *)
    "findIndex", (fun _ ->
      Eq(1, [| 1; 2; 3; 4 |] |> Js.Array.findIndex ((fun n -> n mod 2 = 0) ))
    );
    "findIndexi", (fun _ ->
      Eq(0, [| 1; 2; 3; 4 |] |> Js.Array.findIndexi ((fun _ i -> i mod 2 = 0) ))
    );

    "forEach", (fun _ ->
      let sum = ref 0 in
      let _ = [| 1; 2; 3; |] |> Js.Array.forEach ((fun n -> sum := !sum + n) ) in

      Eq(6, !sum)
    );
    "forEachi", (fun _ ->
      let sum = ref 0 in
      let _ = [| 1; 2; 3; |] |> Js.Array.forEachi ((fun _ i -> sum := !sum + i) ) in

      Eq(3, !sum)
    );

    (* es2015, iterator
    "keys", (fun _ ->
      Eq([| 0; 1; 2 |],
         [| "a"; "b"; "c" |] |> Js.Array.keys |> Js.Array.from)
    );
    *)

    "map", (fun _ ->
      Eq([| 2; 4; 6; 8 |],
         [| 1; 2; 3; 4 |] |> Js.Array.map ((fun n -> n * 2) ))
    );
    "map", (fun _ ->
      Eq([| 0; 2; 4; 6 |],
         [| 1; 2; 3; 4 |] |> Js.Array.mapi ((fun _ i -> i * 2) ))
    );

    "reduce", (fun _ ->
      Eq(-10,
         [| 1; 2; 3; 4 |] |> Js.Array.reduce ((fun acc n -> acc - n) ) 0)
    );
    "reducei", (fun _ ->
      Eq(-6,
         [| 1; 2; 3; 4 |] |> Js.Array.reducei ((fun acc _ i -> acc - i) ) 0)
    );

    "reduceRight", (fun _ ->
      Eq(-10, [| 1; 2; 3; 4 |] |> Js.Array.reduceRight ((fun acc n -> acc - n) ) 0)
    );
    "reduceRighti", (fun _ ->
      Eq(-6, [| 1; 2; 3; 4 |] |> Js.Array.reduceRighti ((fun acc _ i -> acc - i) ) 0)
    );

    "some", (fun _ ->
      Eq(false, [| 1; 2; 3; 4 |] |> Js.Array.some ((fun n ->  (n <= 0)) ))
    );
    "somei", (fun _ ->
      Eq(true, [| 1; 2; 3; 4 |] |> Js.Array.somei ((fun _ i ->  (i <= 0)) ))
    );

    (* es2015, iterator
    "values", (fun _ ->
      Eq([| "a"; "b"; "c" |],
         [| "a"; "b"; "c" |] |> Js.Array.values |> Js.Array.from)
    );
    *)
]

;; Mt.from_pair_suites __FILE__ suites
