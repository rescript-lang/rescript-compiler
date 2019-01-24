(* PR 4470: simplified from OMake's sources *)

module rec DirElt
  : sig
      type t = DirRoot | DirSub of DirHash.t
    end
  = struct
      type t = DirRoot | DirSub of DirHash.t
    end

and DirCompare
  : sig
      type t = DirElt.t
    end
  = struct
      type t = DirElt.t
    end

and DirHash
  : sig
      type t = DirElt.t list
    end
  = struct
      type t = DirCompare.t list
    end
