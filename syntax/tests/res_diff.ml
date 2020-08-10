type change = {
  (* number of lines in new data changed/inserted here. *)
  inserted: int;
	(* number of lines in old data changed/deleted here.  *)
  deleted: int;
	(* line number of 1st deleted line.  *)
  firstDeletedLine: int;
  (* line number of 1st inserted line.  *)
  firstInsertedLine: int;
}

(* A diff is a chain of changes *)
type diff = change list

(* 'a' stands for added, 'd' for deleted and 'c' for changed *)
let changeLetter inserts deletes =
  match inserts, deletes with
  | 0, _ -> 'd'
  | _, 0 -> 'a'
  | _ -> 'c'

let translateLineNumber lnum = lnum + 1

let numberRange a b =
  let a = translateLineNumber a in
  let b = translateLineNumber b in
  if b > a then
    Format.sprintf "%d,%d" a b
  else
    string_of_int b

let printChange change oldArr newArr =
  (* No insertions or deletions means same line *)
  if change.inserted = 0 && change.deleted == 0 then ()
  else
    let lastDeletedLine = (change.firstDeletedLine + change.deleted - 1) in
    let lastInsertedLine = (change.firstInsertedLine + change.inserted - 1) in
    Format.sprintf "%s%c%s"
      (numberRange change.firstDeletedLine lastDeletedLine)
      (changeLetter change.inserted change.deleted)
      (numberRange change.firstInsertedLine lastInsertedLine)
    |> print_endline;

    if change.deleted > 0 then
      for i = change.firstDeletedLine to lastDeletedLine do
        Format.sprintf "< %s" (Array.get oldArr i) |> print_endline;
      done;
    if change.deleted > 0 && change.inserted > 0 then
      print_endline "---";
    if change.inserted > 0 then
      for i = change.firstInsertedLine to lastInsertedLine do
        Format.sprintf "> %s" (Array.get newArr i) |> print_endline;
      done

(* Compute longest common subsequence between oldArr and newArr
 * Uses the dynamic programming approach described on:
 * https://en.wikipedia.org/wiki/Longest_common_subsequence_problem *)
let computeMatrix ~compareFn oldArr oldLen newArr newLen =
  let matrix = Array.make_matrix (oldLen + 1) (newLen + 1) 0 in

  for i = 1 to oldLen do
    for j = 1 to newLen do
      let c = compareFn (Array.unsafe_get oldArr (i - 1)) (Array.unsafe_get newArr (j - 1)) in
      if c = 0 then
        let northEast = Array.unsafe_get (Array.unsafe_get matrix (i - 1)) (j - 1) in
        Array.unsafe_set (Array.unsafe_get matrix i) j (northEast + 1)
      else
        let north = Array.unsafe_get (Array.unsafe_get matrix (i - 1)) j in
        let east = Array.unsafe_get (Array.unsafe_get matrix i) (j - 1) in
        Array.unsafe_set (Array.unsafe_get matrix i) j (max east north)
    done;
  done;

  matrix

(* emits differences between `oldArr` and `newArr` through the `onChange` callback *)
let diff ~onChange ~compareFn oldArr newArr =
  let oldLen = Array.length oldArr in
  let newLen = Array.length newArr in

  (* compute the longest common subsequence *)
  let matrix = computeMatrix ~compareFn oldArr oldLen newArr newLen in

  (* Read the longest common subsequence matrix and compute changes while working
   * backwards through the matrix *)
  let rec walkMatrix i j firstDeletedLine firstInsertedLine =
    if i > 0 && j > 0 then (
      if (Array.unsafe_get oldArr (i - 1)) = (Array.unsafe_get newArr (j - 1)) then (
        onChange {
          firstDeletedLine = i;
          firstInsertedLine = j;
          deleted = firstDeletedLine - i;
          inserted = firstInsertedLine - j;
        };
        let nextI = i - 1 in
        let nextJ = j - 1 in
        walkMatrix nextI nextJ nextI nextJ
      ) else if (Array.unsafe_get (Array.unsafe_get matrix (i - 1)) j) >= (Array.unsafe_get (Array.unsafe_get matrix i) (j - 1)) then (
        walkMatrix (i - 1) j firstDeletedLine firstInsertedLine
      ) else (
        walkMatrix i (j - 1) firstDeletedLine firstInsertedLine
      )
    ) else if j > 0 && i = 0 then (
      walkMatrix i (j - 1) firstDeletedLine firstInsertedLine
    ) else if i > 0 && j = 0 then (
      walkMatrix (i - 1) j firstDeletedLine firstInsertedLine
    ) else (
      onChange {
        firstDeletedLine = i;
        firstInsertedLine = j;
        deleted = firstDeletedLine - i;
        inserted = firstInsertedLine - j
      }
    )
  in
  walkMatrix oldLen newLen oldLen newLen

(* splits both strings in lines and does a line-based diff *)
let diffTwoStrings oldString newString =
  let oldArr = oldString |> String.split_on_char '\n' |> Array.of_list in
  let newArr = newString |> String.split_on_char '\n' |> Array.of_list in
  let script = ref [] in
  diff
    ~onChange:(fun change -> script.contents <- change::script.contents)
    ~compareFn:String.compare
    oldArr
    newArr;
  List.iter (fun change -> printChange change oldArr newArr) script.contents
