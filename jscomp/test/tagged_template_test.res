@module("./tagged_template_lib.js") @variadic external sql: (array<string>, array<string>) => string = "sql"

let table = "users"
let id = "5"

let query = sql`SELECT * FROM ${table} WHERE id = ${id}`

Mt.from_pair_suites(
    "tagged template", 
    list{
        ("it should return a string with the correct interpolations", () => 
            Eq(query, "SELECT * FROM users WHERE id = 5")),
    }
)