include (
  {
    module StringMap = Map.Make({
      type t = string
      let compare = (x: string, y) => compare(x, y)
    })
    @val("console.time") external time: string => unit = ""
    @val("console.timeEnd") external timeEnd: string => unit = ""

    let timing = (label, f) => {
      time(label)
      f()
      timeEnd(label)
    }

    let assertion_test = () => {
      let m = ref(StringMap.empty)
      let count = 1000000
      \"@@"(timing("building", ...), _ =>
        for i in 0 to count {
          m := StringMap.add(Js.Int.toString(i), Js.Int.toString(i), m.contents)
        }
      )
      \"@@"(timing("querying", ...), _ =>
        for i in 0 to count {
          ignore(StringMap.find(Js.Int.toString(i), m.contents))
        }
      )
    }
  }: {
    let assertion_test: unit => unit
  }
)
