include (
  {
    module IntMap = Map.Make({
      type t = int
      let compare = (x: t, y) => compare(x, y)
    })

    let empty = IntMap.empty

    let m = List.fold_left(
      (acc, (k, v)) => IntMap.add(k, v, acc),
      empty,
      list{(10, 'a'), (3, 'b'), (7, 'c'), (20, 'd')},
    )

    /* external log : 'a -> unit = "" [@@bs.val "console.log"] */

    let assert_test = () =>
      if IntMap.find(10, m) == 'a' {
        prerr_endline("hi")
      } else {
        /* log ('a', "succeed") */

        prerr_endline("hi")
      }
  }: {}
)
