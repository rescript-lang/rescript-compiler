/*** Node Path API */

@module("path") external basename: string => string = "basename"
@module("path") external basename_ext: (string, string) => string = "basename"

@module("path") external delimiter: string = "delimiter"

@module("path") external dirname: string => string = "dirname"

@module("path") external dirname_ext: (string, string) => string = "dirname"

type pathObject = {"dir": string, "root": string, "base": string, "name": string, "ext": string}

@module("path") external format: pathObject => string = "format"

@module("path") external isAbsolute: string => bool = "isAbsolute"

/* TODO: improve after we support [@bs.rest] calling convention */
@module("path") external join2: (string, string) => string = "join"

@module("path") @variadic external join: array<string> => string = "join"

@module("path") external normalize: string => string = "normalize"

/* TODO: check if there is an exception raised */
@module("path") external parse: string => pathObject = "parse"

/* TODO: provide bindings to `path.posix` */

@module("path") external relative: (~from: string, ~to_: string, unit) => string = "relative"

/* TODO: improve after rest calling convention */
@module("path") external resolve: (string, string) => string = "resolve"

@module("path") external sep: string = "sep"

/* TODO: provides `path.win32` */
