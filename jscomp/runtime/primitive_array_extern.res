@get external length: array<'a> => int = "length"
@get_index external getUnsafe: (array<'a>, int) => 'a = ""
@set_index external setUnsafe: (array<'a>, int, 'a) => unit = ""
