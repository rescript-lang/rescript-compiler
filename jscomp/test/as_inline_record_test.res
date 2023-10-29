type user =
  | User({
      @as("renamed")
      name: string,
    })

let user = User({name: "Corentin"})

let getName = t =>
  switch t {
  | User({name}) => name
  }
