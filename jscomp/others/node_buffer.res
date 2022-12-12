/*** Node Buffer API */

type t = Node.buffer

@val external isBuffer: 'a => bool = "Buffer.isBuffer"

@val external fromString: string => t = "Buffer.from"

type encoding = [#ascii | #utf8 | #utf16le | #usc2 | #base64 | #latin1 | #binary | #hex]
@val @scope("Buffer") external fromStringWithEncoding: (string, encoding) => t = "from"

@send external toString: t => string = "toString"

@send external toStringWithEncoding: (t, encoding) => string = "toString"

@val external concat: array<t> => t = "Buffer.concat"
