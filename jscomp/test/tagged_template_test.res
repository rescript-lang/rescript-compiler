@module("./tagged_template_lib.js") @variadic
external sql: (array<string>, array<string>) => string = "sql"

let table = "users"
let id = "5"

let query = sql`SELECT * FROM ${table} WHERE id = ${id}`

let foo = (strings, values) => {
  let res = ref("")
  let valueCount = Array.length(values)
  for i in 0 to valueCount - 1 {
    res := res.contents ++ strings[i] ++ string_of_int(values[i] * 10)
  }
  res.contents ++ strings[valueCount]
}

let res = foo`| 5 * 10 = ${5} |`

Mt.from_pair_suites(
  "tagged templates",
  list{
    (
      "with externals, it should return a string with the correct interpolations",
      () => Eq(query, "SELECT * FROM users WHERE id = 5"),
    ),
    (
      "with rescript function, it should return a string with the correct interpolations",
      () => Eq(res, "| 5 * 10 = 50 |"),
    ),
    (
      "a template literal tagged with json should generate a regular string interpolation for now",
      () => Eq(json`some random ${"string"}`, "some random string"),
    ),
  },
)