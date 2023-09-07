@obj
external func: (
  ~param: string,
  ~polyParam: @unwrap
  [
    | #Str(string)
    | #Int(int)
  ],
) => _ = ""

let a = func(~param="", ~polyParam=#Str("hey"))

let b = func(~param="", ~polyParam=#Int(5))

@obj
external funcOpt: (
  ~param: string,
  ~polyParam: @unwrap
  [
    | #Str(string)
    | #Int(int)
  ]=?,
  unit,
) => _ = ""

let c = funcOpt(~param="", ~polyParam=#Int(5), ())
