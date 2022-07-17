let processCmt cmt =
  Log_.Color.forceColor := true;
  let config = Paths.readConfig ~namespace:(cmt |> Paths.findNameSpace) in
  if !Debug.basic then Log_.item "Add %s\n" cmt;
  cmt |> GenTypeMain.processCmtFile ~config
