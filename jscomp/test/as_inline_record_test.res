type user =
  | User({
      @as("renamed")
      name: string,
    })

let user = User({name: "Corentin"})
