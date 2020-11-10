array->Js.Array2.forEach(((value, height)) =>
  add(rbt, value, ~height)->ignore
)
someArray->Js.Array2.forEach(((value, height)) =>  add(rbt, value, ~height)->ignore)
someArraaaaaaayWithAVeryLooooooooooooooooooooooooooooooooooooooongName->Js.Array2.forEach(((value, height)) =>  add(rbt, value, ~height)->ignore)

let make = (arr, ~compare) => {
  let rbt = make(~compare)
  array->Js.Array2.forEach(((value, height)) => add(rbt,value, ~height)->ignore)
  rbt
}


Thing.map(foo,(arg1, arg2) => MyModuleBlah.toList(argument))
Thing.map(foo, bar, baz, (aaaaaaaaaaarg1, aaaaarg2) => MyModuleBlah.toList(argument))
Thing.map(fooooooooooooooooooo, bar, baz, (abc, z) => MyModuleBlah.toList(argument))
Thing.map(fooooooooooooooooooo, baaaaaaaaaaaaaaaaaar, baaaaaaaaaaaaaaaaaaaaaaaaaz, (abc, z) => MyModuleBlah.toList(argument))
Fooooooooooooooooooo.baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa(
  baaaz, quuuuux =>
  something(here)
);


Fooooooooooooooooooo.baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa(
  baaaz, quuuuux => {
    let a = 1
    let b = 2
    a + b
  }
);

let make = (arr, ~compare) => {
  let rbt = make(~compare)
  array->Js.Array2.forEach(((value, height)) => add(rbt, value, ~height)->ignore
  )
  rbt
}

let tbl = data->Js.Array2.reduce(
        (map, curr) => {
          let (website, user) = curr
          if map->Belt.Map.String.has(website) {
            let set =
              map->Belt.Map.String.getExn(website)->Belt.Set.Int.add(user)
            map->Belt.Map.String.set(website, set)
          } else {
            let set = Belt.Set.Int.empty->Belt.Set.Int.add(user)
            map->Belt.Map.String.set(website, set)
          }
        },
        Belt.Map.String.empty,
      )

let _ = {
  let similarity = (a, b) => {
      // nbr of users in common/number of users who have visited either in total
      let tbl =
        data->Js.Array2.reduce(
          (map, curr) => {
            let (website, user) = curr
            if map->Belt.Map.String.has(website) {
              let set = map->Belt.Map.String.getExn(website)->Belt.Set.Int.add(user)
              map->Belt.Map.String.set(website, set)
            } else {
              let set = Belt.Set.Int.empty->Belt.Set.Int.add(user)
              map->Belt.Map.String.set(website, set)
            }
          },
          Belt.Map.String.empty,
        )
      ()
    }
    ()
  }

let trees = possibilities->Belt.Array.mapU((. combination) =>
  combination->Belt.Array.reduceU(Nil, (. tree, curr) => tree->insert(curr))
)

let set = mapThatHasAVeryLongName->Belt.Map.String.getExn(website)->Belt.Set.Int.add(user)

let add = (y: coll, e: key) =>
  if (List.exists(x => eq(x, e), y)) {
    y;
  } else {
    list{e, ...y};
  };

let add2 = (y: coll, e: key) =>
  if (List.exists(y, x => eq(x, e) )) {
    y;
  } else {
    list{e, ...y};
  };


let add2 = (y: coll, e: key) =>
  if possibilities->Belt.Array.mapU((. combination) =>
  combination->Belt.Array.reduceU(Nil, (. tree, curr) => tree->insert(curr))
) {
    y;
  } else {
    list{e, ...y};
  };

let test =
moduleTypeDeclaration |> TranslateSignature.translateModuleTypeDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       ) |> doStuff(~x, ~y)

// labelled arguments
// callback in last position
// case 1
Thing.map(foo, ~fn=(arg1, arg2) => MyModuleBlah.toList(argument))
Thing.map(foo, ~fn=?(arg1, arg2) => MyModuleBlah.toList(argument))

// case 2
Thing.map(longArgument, veryLooooongArgument, ~fn=(arg1, arg2) =>
  MyModuleBlah.toList(argument)
)
Thing.map(longArgument, veryLooooongArgument, ~fn=?(arg1, arg2) =>
  MyModuleBlah.toList(argument)
)

// case 3
Thing.map(
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg1,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg2,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg3,
  ~fn=(param1, parm2) => doStuff(param1, parm2)
)
Thing.map(
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg1,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg2,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg3,
  ~fn=?(param1, parm2) => doStuff(param1, parm2)
)

// callback in first position
// case 1
Thing.map(~fn=(arg1, arg2) => MyModuleBlah.toList(argument), foo)
Thing.map(~fn=?(arg1, arg2) => MyModuleBlah.toList(argument), foo)

// case 2
Thing.map(~fn=(arg1, arg2) => {
  MyModuleBlah.toList(argument)
}, longArgument, veryLooooongArgument)
Thing.map(~fn=?(arg1, arg2) => {
  MyModuleBlah.toList(argument)
}, longArgument, veryLooooongArgument)

// case 3
Thing.map(
  ~fn=(param1, parm2) => doStuff(param1, parm2),
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg2,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg3,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg4,
)
Thing.map(
  ~fn=?(param1, parm2) => doStuff(param1, parm2),
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg2,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg3,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaarg4,
)

List.iter(x => switch x {
| ({txt: Lident(name)}, x) => field(name, x)
| _ => ()
}, fields)

foo(key => {
  switch bla {
  // cancellation =D
  | None => true
  }
})

foo(_ => {
  switch bar {
    | None => ()
    | Some(_) =>
      a := b
      Js.log("hi")
  }
})

foo(list => list())
foo(\"switch" => \"switch"())

// the [] of the array should break
[
  fn(x => {
    let _ = x
  }),
  fn(y => {
    let _ = y
  }),
  fn(z => {
    let _ = z
  })
]

// similar, the jsx tree should be broken over multiple lines
let f = () => {
  <div>
    {switch user.commited {
    | NotAsked => React.null
    | Loading => "Loading ..."->React.string
    | Done(Error(_)) => "Error"->React.string
    | Done(Ok(users)) => <> <ul> {users->Array.map(user => {
            <li key=user.id> {user.username->React.string} </li>
          })->React.array} </ul> {reloadableUser.last->AsyncData.isLoading
          ? "Loading next page"->React.string
          : React.null} <button onClick={_ => setPage(x => x + 1)}>
          {"Load next page"->React.string}
        </button> </>
    }}
  </div>
}

myPromise->Js.Promise.then_(value => {
  Js.log(value)
  Js.Promise.resolve(value + 2)
}, _)->Js.Promise.then_(value => {
  Js.log(value)
  Js.Promise.resolve(value + 3)
}, _)->Js.Promise.catch(err => {
  Js.log2("Failure!!", err)
  Js.Promise.resolve(-2)
}, _)
