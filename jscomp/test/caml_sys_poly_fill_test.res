let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let () = {
  eq(
    __LOC__,
    "X",
    {
      Node_process.putEnvVar(__MODULE__, "X")
      let v = Sys.getenv(__MODULE__)
      Node_process.deleteEnvVar(__MODULE__)
      v
    },
  )
  eq(
    __LOC__,
    "Y",
    {
      Node_process.putEnvVar(__MODULE__, "Y")
      let v = Sys.getenv(__MODULE__)
      Node_process.deleteEnvVar(__MODULE__)
      v
    },
  )
  eq(
    __LOC__,
    "Z",
    {
      Node_process.deleteEnvVar(__MODULE__)
      let v = try Sys.getenv(__MODULE__) catch {
      | Not_found => "Z"
      }
      v
    },
  )
}

Js.log((Sys.getcwd(), Sys.time(), Sys.argv, Sys.executable_name))

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
