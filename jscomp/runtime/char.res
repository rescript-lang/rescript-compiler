// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

type t = char

external code: t => int = "%identity"
external unsafe_chr: int => t = "%identity"

let chr = unsafe_chr
