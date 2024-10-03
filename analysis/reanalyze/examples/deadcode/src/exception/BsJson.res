@raise(DecodeError)
let testBsJson = x => Json_decode.string(x)

@raise(DecodeError)
let testBsJson2 = x => Json.Decode.string(x)
