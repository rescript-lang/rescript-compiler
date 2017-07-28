(**
   Test the stream example .
*)
type stream = Stream of (int * stream Lazy.t)
;;

exception End_of_stream
;;

let make_stream_up_to n =
  let rec loop i =
    if i = n then Stream (i, lazy (raise End_of_stream))
    else Stream (i, lazy (loop (i + 1)))
  in loop 0
;;

let stream_get (Stream (x, s)) = (x, Lazy.force s)
;;

let rec iter_stream_match f s = 
  match stream_get s
  with exception End_of_stream -> ()
  | (x, s') ->
    begin
      f x;
      iter_stream_match f s'
    end
;;
      
let test_iter_stream =
  let limit = 10000000 in
  try
    iter_stream_match ignore (make_stream_up_to limit);
    print_endline "iter_stream with handler case (match) is tail recursive"
  with Stack_overflow ->
    assert false
;;
