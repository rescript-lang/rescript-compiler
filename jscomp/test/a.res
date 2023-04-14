include (
  {
    module M = (
      S: {
        let add: (int, int) => int
      },
    ) => {
      let u = S.add(1, 2)
    }
    module H = M({
      let add = (x, y) => x + y
    })

    include List
    module N = List
    let v = N.length

    module Make = (U: Set.OrderedType) => {
      include U
      let v = compare
    }

    module X = Make(String)
    module U = Make(Test_order)

    include N
    /* let v = "xhg" */
    /* let () = v.[0] <- 'a' */
  }: {}
)

/* [%%bs.cast.x: 'a -> 'b  ] */
/* external f : int -> (int -> int) = "%identity" */
