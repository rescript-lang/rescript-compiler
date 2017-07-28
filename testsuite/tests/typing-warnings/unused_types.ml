module Unused : sig
end = struct
  type unused = int
end
;;

module Unused_nonrec : sig
end = struct
  type nonrec used = int
  type nonrec unused = used
end
;;

module Unused_rec : sig
end = struct
  type unused = A of unused
end
;;
