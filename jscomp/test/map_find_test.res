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

    module SMap = Map.Make({
      type t = string
      let compare = (x: t, y) => compare(x, y)
    })

    let s = List.fold_left(
      (acc, (k, v)) => SMap.add(k, v, acc),
      SMap.empty,
      list{("10", 'a'), ("3", 'b'), ("7", 'c'), ("20", 'd')},
    )
    @val("console.log") external log: 'a => unit = ""

    \"@@"(
      Mt.from_pair_suites(__MODULE__),
      list{("int", _ => Eq(IntMap.find(10, m), 'a')), ("string", _ => Eq(SMap.find("10", s), 'a'))},
    )
  }: {}
)
