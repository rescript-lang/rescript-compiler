let rec a = lazy b and b = 3;;

let rec e = lazy (fun _ -> f) and f = ();;
