type utf8Test = {codepoint: int; str: string; size: int}

let utf8CodePointTests =
  [|
    {codepoint = 0x00; str = "\x00"; size = 1};
    {codepoint = 0x01; str = "\x01"; size = 1};
    {codepoint = 0x7e; str = "\x7e"; size = 1};
    {codepoint = 0x7f; str = "\x7f"; size = 1};
    {codepoint = 0x0080; str = "\xc2\x80"; size = 2};
    {codepoint = 0x0081; str = "\xc2\x81"; size = 2};
    {codepoint = 0x00bf; str = "\xc2\xbf"; size = 2};
    {codepoint = 0x00c0; str = "\xc3\x80"; size = 2};
    {codepoint = 0x00c1; str = "\xc3\x81"; size = 2};
    {codepoint = 0x00c8; str = "\xc3\x88"; size = 2};
    {codepoint = 0x00d0; str = "\xc3\x90"; size = 2};
    {codepoint = 0x00e0; str = "\xc3\xa0"; size = 2};
    {codepoint = 0x00f0; str = "\xc3\xb0"; size = 2};
    {codepoint = 0x00f8; str = "\xc3\xb8"; size = 2};
    {codepoint = 0x00ff; str = "\xc3\xbf"; size = 2};
    {codepoint = 0x0100; str = "\xc4\x80"; size = 2};
    {codepoint = 0x07ff; str = "\xdf\xbf"; size = 2};
    {codepoint = 0x0400; str = "\xd0\x80"; size = 2};
    {codepoint = 0x0800; str = "\xe0\xa0\x80"; size = 3};
    {codepoint = 0x0801; str = "\xe0\xa0\x81"; size = 3};
    {codepoint = 0x1000; str = "\xe1\x80\x80"; size = 3};
    {codepoint = 0xd000; str = "\xed\x80\x80"; size = 3};
    {codepoint = 0xd7ff; str = "\xed\x9f\xbf"; size = 3};
    {codepoint = 0xe000; str = "\xee\x80\x80"; size = 3};
    {codepoint = 0xfffe; str = "\xef\xbf\xbe"; size = 3};
    {codepoint = 0xffff; str = "\xef\xbf\xbf"; size = 3};
    {codepoint = 0x10000; str = "\xf0\x90\x80\x80"; size = 4};
    {codepoint = 0x10001; str = "\xf0\x90\x80\x81"; size = 4};
    {codepoint = 0x40000; str = "\xf1\x80\x80\x80"; size = 4};
    {codepoint = 0x10fffe; str = "\xf4\x8f\xbf\xbe"; size = 4};
    {codepoint = 0x10ffff; str = "\xf4\x8f\xbf\xbf"; size = 4};
    {codepoint = 0xFFFD; str = "\xef\xbf\xbd"; size = 3};
  |]

let surrogateRange =
  [|
    {codepoint = 0xFFFD; str = "\xed\xa0\x80"; size = 1};
    {codepoint = 0xFFFD; str = "\xed\xbf\xbf"; size = 1};
  |]

let testDecode () =
  Array.iter
    (fun t ->
      let len = String.length t.str in
      let codepoint, size = Res_utf8.decodeCodePoint 0 t.str len in
      assert (codepoint = t.codepoint);
      assert (size = t.size))
    utf8CodePointTests

let testDecodeSurrogateRange () =
  Array.iter
    (fun t ->
      let len = String.length t.str in
      let codepoint, size = Res_utf8.decodeCodePoint 0 t.str len in
      assert (codepoint = t.codepoint);
      assert (size = t.size))
    surrogateRange

let testEncode () =
  Array.iter
    (fun t ->
      let encodedString = Res_utf8.encodeCodePoint t.codepoint in
      assert (encodedString = t.str))
    utf8CodePointTests

let validCodePointsTests =
  [|
    (0, true);
    (Char.code 'e', true);
    (Res_utf8.max, true);
    (0xD7FF, true);
    (0xD800, false);
    (0xDFFF, false);
    (0xE000, true);
    (Res_utf8.max + 1, false);
    (-1, false);
  |]

let testIsValidCodePoint () =
  Array.iter
    (fun (codePoint, t) -> assert (Res_utf8.isValidCodePoint codePoint = t))
    validCodePointsTests

let run () =
  testDecode ();
  testDecodeSurrogateRange ();
  testEncode ();
  testIsValidCodePoint ();
  print_endline "✅ utf8 tests"
