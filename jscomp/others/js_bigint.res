/*** JavaScript BigInt API */

type t = bigint

external \"~-": t => t = "%negbigint"
external \"~+": t => t = "%identity"
external \"+": (t, t) => t = "%addbigint"
external \"-": (t, t) => t = "%subbigint"
external \"*": (t, t) => t = "%mulbigint"
external \"/": (t, t) => t = "%divbigint"
external mod: (t, t) => t = "%modbigint"
