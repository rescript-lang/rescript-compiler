type status = On | Off

@@jsxConfig({version: 4, mode: "automatic"})

@react.component
let make = (~status: status, ~name: string) => {
  ignore(status)
  ignore(name)
  React.null
}
