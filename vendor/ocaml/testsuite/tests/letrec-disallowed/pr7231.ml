let rec r = let rec x () = r and y () = x () in y () in r "oops";; 
