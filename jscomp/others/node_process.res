/*** Node Process API */

type t = {
  "argv": array<string>,
  "arch": string,
  @meth
  "abort": unit => unit,
  @meth
  "chdir": string => unit,
  @meth
  "cwd": unit => string,
  @meth
  "disconnect": unit => unit,
  "platform": string,
  "env": Js_dict.t<string> /* ocamldep sucks which can not map `Js.Dic.t` to `Js_dict.t` */,
}

@module external process: t = "process"
@module("process") external argv: array<string> = "argv"
@module("process") external exit: int => 'a = "exit"
@module("process") external cwd: unit => string = "cwd"

/**
  The process.uptime() method returns the number of seconds
  the current Node.js process has been running.)
*/
@send
external uptime: (t, unit) => float = "uptime"

/**
  Note that
  `process.env.X = undefined` will result in
  `process.env.X = \"undefined\"`
  The only sane way to do it is using `delete`
*/
let putEnvVar = (key, var: string) => Js_dict.set(process["env"], key, var)

let deleteEnvVar = s => Js_dict.unsafeDeleteKey(. process["env"], s)
