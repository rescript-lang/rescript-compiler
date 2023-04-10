open Mt
module Int = {
  type t = int
  let compare = (x: int, y: int) => Pervasives.compare(x, y)
}
module Int_map = Map.Make(Int)
module String_map = Map.Make(String)

let of_list = kvs => List.fold_left((acc, (k, v)) => Int_map.add(k, v, acc), Int_map.empty, kvs)

let int_map_suites = {
  open Mt
  open Int_map
  list{
    (
      "add",
      _ => {
        let v = of_list(list{(1, '1'), (2, '3'), (3, '4')})
        Eq(cardinal(v), 3)
      },
    ),
    (
      "equal",
      _ => {
        let v = of_list(list{(1, '1'), (2, '3'), (3, '4')})
        let u = of_list(list{(2, '3'), (3, '4'), (1, '1')})
        Eq(compare(Pervasives.compare, u, v), 0)
      },
    ),
    (
      "equal2",
      _ => {
        let v = of_list(list{(1, '1'), (2, '3'), (3, '4')})
        let u = of_list(list{(2, '3'), (3, '4'), (1, '1')})
        Eq(true, equal((x, y) => x == y, u, v))
      },
    ),
    (
      "iteration",
      _ => {
        let m = ref(String_map.empty)
        let count = 1_0000
        for i in 0 to count {
          m := String_map.add(string_of_int(i), string_of_int(i), m.contents)
        }
        let v = ref(-1)
        for i in 0 to count {
          if String_map.find(string_of_int(i), m.contents) !== string_of_int(i) {
            v := i
          }
        }
        Eq(v.contents, -1)
      },
    ),
  }
}
Mt.from_pair_suites(__MODULE__, int_map_suites)
