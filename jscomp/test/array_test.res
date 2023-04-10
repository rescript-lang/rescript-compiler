open Mt

module type ARRAY = module type of Array
module Make = (Array: ARRAY) => {
  let starts_with = (xs, prefix, p) => {
    module X = {
      exception H
    }
    let (len1, len2) = {
      open Array
      (length(xs), length(prefix))
    }
    if len2 > len1 {
      false
    } else {
      try {
        for i in 0 to len2 - 1 {
          if \"@@"(not, p(xs[i], prefix[i])) {
            raise(X.H)
          }
        }
        true
      } catch {
      | X.H => false
      }
    }
  }

  let is_sorted = x => {
    let len = Array.length(x)
    let rec aux = i =>
      if i >= len - 1 {
        true
      } else if x[i] < x[i + 1] {
        aux(i + 1)
      } else {
        false
      }
    aux(0)
  }

  let array_suites = {
    open Mt
    list{
      ("init", _ => Eq(Array.init(5, x => x), [0, 1, 2, 3, 4])),
      (
        "toList",
        _ => {
          let aux = (xs: list<(array<int>, list<int>)>) =>
            List.fold_left((acc, (x, y)) => list{(Array.to_list(x), y), ...acc}, list{}, xs)

          let (a, b) = \"@@"(List.split, aux(list{([], list{})}))
          Eq(a, b)
        },
      ),
      ("concat", _ => Eq([0, 1, 2, 3, 4, 5], Array.concat(list{[0, 1, 2], [3, 4], [], [5]}))),
      (
        "make",
        _ => Eq(
          (Array.make(100, 'a'), Array.make_float(100)),
          (Array.init(100, _ => 'a'), Array.init(100, _ => 0.)),
        ),
      ),
      ("sub", _ => Eq(Array.sub([0, 1, 2, 3, 4], 2, 2), [2, 3])),
      (
        "blit",
        _ => {
          let u = [100, 0, 0]
          let v = Array.init(3, x => x * 2)
          let () = Array.blit(v, 1, u, 1, 2)
          Eq(([0, 2, 4], [100, 2, 4]), (v, u))
        },
      ),
      (
        __LOC__,
        {
          open Array
          _ => {
            let a0 = init(100, i => i * 1)
            blit(a0, 10, a0, 5, 20)
            Eq(
              true,
              starts_with(
                a0,
                [
                  0,
                  1,
                  2,
                  3,
                  4,
                  10,
                  11,
                  12,
                  13,
                  14,
                  15,
                  16,
                  17,
                  18,
                  19,
                  20,
                  21,
                  22,
                  23,
                  24,
                  25,
                  26,
                  27,
                  28,
                ],
                \"=",
              ),
            )
          }
        },
      ),
      (
        __LOC__,
        {
          open Array
          _ => {
            let a0 = init(100, i => i * 1)
            blit(a0, 5, a0, 10, 20)
            Eq(
              true,
              starts_with(
                a0,
                [
                  0,
                  1,
                  2,
                  3,
                  4,
                  5,
                  6,
                  7,
                  8,
                  9,
                  5,
                  6,
                  7,
                  8,
                  9,
                  10,
                  11,
                  12,
                  13,
                  14,
                  15,
                  16,
                  17,
                  18,
                  19,
                  20,
                ],
                \"=",
              ),
            )
          }
        },
      ),
      ("make", _ => Eq(Array.make(2, 1), [1, 1])),
      (
        "sort",
        _ => {
          let u = [3, 0, 1]
          Array.sort((x: int, y) => Pervasives.compare(x, y), u)
          Eq([0, 1, 3] == u, true) /* seems [assert.deepEqual] does not do the right job.. */
        },
      ),
      (
        "sort_large",
        _ => {
          let v = Array.init(4, i => mod(i, 17))
          Array.sort((x: int, y) => compare(x, y), v)
          Eq(true, is_sorted(v))
        },
      ),
    }
  }
}

from_pair_suites(
  __MODULE__,
  {
    module Array_test = Make(Array)
    Array_test.array_suites
  },
)
