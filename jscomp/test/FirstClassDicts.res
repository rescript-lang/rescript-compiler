module CreateAndLookup = {
  type myDict = {name?: string, anyOtherField?: int}

  let test_create: myDict = {name: "hello", something: 5}

  let test_lookup = (d: myDict) : option<int> => d.something
}


module Update = {
  type myDict = {name?: string, mutable anyOtherField?: int}

  let test_update = (d: myDict) => d.something = Some(10)
}


module PatternMatching = {
  type myDict = {name?: string, anyOtherField?: int}

  let tst1 = (d: myDict) =>
    switch d {
    | {name: n, something: i} => String.length(n) + i
    | {name: n} => String.length(n)
    | {something: i} => i
    | _ => 0
    }

  let tst2 = (d: myDict) =>
    switch d {
    | {name: _, a: i, b: j} => i + j
    | _ => 0
    }
}
