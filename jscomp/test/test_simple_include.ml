include Array

include struct
  ;;
  assert (1 + 2 = 3)

  let a = 3
end

module N = struct
  (* ;; prerr_endline "hello " *)

  ;;
  assert (1 + 2 = 3)

  let a = 3
  let v = ref 32

  ;;
  v := 0
end

module NN = struct let a = 3 let v = ref 32 end
include N
