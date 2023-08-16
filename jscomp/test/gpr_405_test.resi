module type G = {
  type t
  module V: {
    /*** Vertices are {!COMPARABLE}. */

    type t

    let compare: (t, t) => int
    let hash: t => int
    let equal: (t, t) => bool

    type label
    let create: label => t
    let label: t => label
  }
  let succ: (t, V.t) => list<V.t>
}

module Make: (G: G) =>
{
  let min_cutset: (G.t, G.V.t) => list<G.V.t>
}
