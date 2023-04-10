open Js_null_undefined

let suites = {
  open Mt
  list{
    ("toOption - null", _ => Eq(None, null |> toOption)),
    ("toOption - undefined", _ => Eq(None, undefined |> toOption)),
    ("toOption - empty", _ => Eq(None, undefined |> toOption)),
    (__LOC__, _ => Eq(Some("foo"), return("foo") |> toOption)),
    ("return", _ => Eq(Some("something"), return("something") |> toOption)),
    ("test - null", _ => Eq(true, null |> isNullable)),
    ("test - undefined", _ => Eq(true, undefined |> isNullable)),
    ("test - empty", _ => Eq(true, undefined |> isNullable)),
    (__LOC__, _ => Eq(true, return() |> isNullable)),
    ("bind - null", _ => StrictEq(null, bind(null, (. v) => v))),
    ("bind - undefined", _ => StrictEq(undefined, bind(undefined, (. v) => v))),
    ("bind - empty", _ => StrictEq(undefined, bind(undefined, (. v) => v))),
    ("bind - 'a", _ => Eq(return(4), bind(return(2), (. n) => n * 2))),
    (
      "iter - null",
      _ => {
        let hit = ref(false)
        let _ = iter(null, (. _) => hit := true)
        Eq(false, hit.contents)
      },
    ),
    (
      "iter - undefined",
      _ => {
        let hit = ref(false)
        let _ = iter(undefined, (. _) => hit := true)
        Eq(false, hit.contents)
      },
    ),
    (
      "iter - empty",
      _ => {
        let hit = ref(false)
        let _ = iter(undefined, (. _) => hit := true)
        Eq(false, hit.contents)
      },
    ),
    (
      "iter - 'a",
      _ => {
        let hit = ref(0)
        let _ = iter(return(2), (. v) => hit := v)
        Eq(2, hit.contents)
      },
    ),
    ("fromOption - None", _ => Eq(undefined, None |> fromOption)),
    ("fromOption - Some", _ => Eq(return(2), Some(2) |> fromOption)),
    ("null <> undefined", _ => Ok(null != undefined)),
    ("null <> empty", _ => Ok(null != undefined)),
    ("undefined = empty", _ => Ok(undefined == undefined)),
    (
      __LOC__,
      _ => Ok({
        let null = 3
        !Js.isNullable(Js.Nullable.return(null))
      }),
    ),
  }
}
Mt.from_pair_suites(__MODULE__, suites)
