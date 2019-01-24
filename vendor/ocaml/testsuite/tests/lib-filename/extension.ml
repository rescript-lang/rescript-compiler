let () =
  let test f e =
    assert(Filename.extension f = e);
    assert(Filename.extension ("foo/" ^ f) = e);
    assert(f = Filename.remove_extension f ^ Filename.extension f)
  in
  test "" "";
  test "foo" "";
  test "foo.txt" ".txt";
  test "foo.txt.gz" ".gz";
  test ".foo" "";
  test "." "";
  test ".." "";
  test "foo..txt" ".txt"
