type t

@new external fromBuffer: ArrayBuffer.t => t = "DataView"
@new external fromBufferToEnd: (ArrayBuffer.t, ~byteOffset: int) => t = "DataView"
@new
external fromBufferWithRange: (ArrayBuffer.t, ~byteOffset: int, ~length: int) => t = "DataView"

@get external buffer: t => ArrayBuffer.t = "buffer"
@get external byteLength: t => int = "byteLength"
@get external byteOffset: t => int = "byteOffset"

@send external getInt8: t => int = "getInt8"
@send external getUint8: t => int = "getUint8"
@send external getInt16: t => int = "getInt16"
@send external getUint16: t => int = "getUint16"
@send external getInt32: t => int = "getInt32"
@send external getUint32: t => int = "getUint32"

@send external getFloat32: t => float = "getFloat32"
@send external getFloat64: t => float = "getFloat64"

@send external getBigInt64: t => bigint = "getBigInt64"
@send external getBigUint64: t => bigint = "getBigUint64"

@send external setInt8: (t, int) => unit = "setInt8"
@send external setUint8: (t, int) => unit = "setUint8"
@send external setInt16: (t, int) => unit = "setInt16"
@send external setUint16: (t, int) => unit = "setUint16"
@send external setInt32: (t, int) => unit = "setInt32"
@send external setUint32: (t, int) => unit = "setUint32"

@send external setFloat32: (t, float) => unit = "setFloat32"
@send external setFloat64: (t, float) => unit = "setFloat64"

@send external setBigInt64: (t, bigint) => unit = "setBigInt64"
@send external setBigUint64: (t, bigint) => unit = "setBigUint64"
