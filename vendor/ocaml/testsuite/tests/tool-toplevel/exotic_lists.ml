module L = struct
        type ('a,'b) t = [] | (::) of 'a * ('b,'a) t
end;;
L.[([1;2]:int list);"2";[3;4];"4";[5]];;
open L;;
[1;"2";3;"4";5];;

module L = struct
        type 'a t = 'a list = [] | (::) of 'a * 'a t
end;;
L.[[1];[2];[3];[4];[5]];;
open L;;
[1;2;3;4;5];;


