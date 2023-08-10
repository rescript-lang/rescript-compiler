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

module Make = (G: G) => {
  module H = Hashtbl.Make(G.V)

  let find_default = (htbl, x) =>
    try H.find(htbl, x) catch {
    | Not_found => false
    }

  let min_cutset = (gr, first_node) => {
    let n_labels = H.create(97)
    let l_labels = H.create(97)

    let already_processed = H.create(97)
    let is_already_processed = x => find_default(already_processed, x)

    let on_the_stack = H.create(97)
    let is_on_the_stack = x => find_default(on_the_stack, x)
    let cut_set = ref(list{})
    let counter = ref(1)

    let rec step2 = (top, rest_of_stack) => {
      assert(!is_already_processed(top))
      assert(!is_on_the_stack(top))
      H.add(on_the_stack, top, true)
      H.add(n_labels, top, counter.contents)
      counter := counter.contents + 1
      H.add(l_labels, top, 0)
      H.add(already_processed, top, true)
      step3(G.succ(gr, top), top, rest_of_stack)
    }

    and step3 = (successors, top, rest_of_stack) =>
      switch successors {
      | list{successor, ...other_successors} =>
        if !is_already_processed(successor) {
          /* step 4 */
          step2(successor, list{(top, successors), ...rest_of_stack})
        } else {
          /* step 5 */

          let x = if is_on_the_stack(successor) {
            H.find(n_labels, successor)
          } else {
            H.find(l_labels, successor)
          }

          H.add(l_labels, top, max(H.find(l_labels, top), x))
          step3(other_successors, top, rest_of_stack)
        }

      | list{} =>
        /* step 7 */
        if H.find(l_labels, top) == H.find(n_labels, top) {
          cut_set := list{top, ...cut_set.contents}
          H.add(l_labels, top, 0)
        }

        /* check added between algorithms C and D */
        if H.find(l_labels, top) > H.find(n_labels, top) {
          raise(Invalid_argument("Graph.Mincut: graph not reducible"))
        } else {
          /* step 8 */
          switch rest_of_stack {
          | list{} => cut_set.contents /* SUCCESS */
          | list{(new_top, new_successors), ...new_tail} =>
            H.add(on_the_stack, top, false)
            H.add(l_labels, new_top, max(H.find(l_labels, top), H.find(l_labels, new_top)))
            step3(new_successors, new_top, new_tail)
          }
        }
      }

    /* step 2 */
    step2(first_node, list{})
  }
}
