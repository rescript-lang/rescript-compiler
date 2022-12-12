/*** Node Child Process API */

type option

@obj external option: (~cwd: string=?, ~encoding: string=?, unit) => option = ""

/* TODO: when no option it would return buffer */
@module("child_process") external execSync: (string, option) => string = "execSync"

/*
  Note we have to make it abstract type, since if you declare it as
  `< pid : float > Js.t`, then you will create other external
  functions which will work with this type too, it is not what you want
*/
type spawnResult

@module("child_process") external spawnSync: string => spawnResult = "spawnSync"

external readAs: spawnResult => {
  "pid": int,
  "status": Js.null<int>,
  "signal": Js.null<string>,
  "stdout": Js.null<Node.string_buffer>,
  "stderr": Js.null<Node.string_buffer>,
} = "%identity"
