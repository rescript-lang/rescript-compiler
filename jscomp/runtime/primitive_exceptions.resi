exception Error = JsError
type js_error = {cause: exn}

// TODO: remove legacy exn representation
type t = {@as("RE_EXN_ID") id: string}
let create: string => string
let internalToException: unknown => exn
