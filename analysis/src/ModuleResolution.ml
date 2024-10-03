let ( /+ ) = Filename.concat

let rec resolveNodeModulePath ~startPath name =
  let path = startPath /+ "node_modules" /+ name in
  if Files.exists path then Some path
  else if Filename.dirname startPath = startPath then None
  else resolveNodeModulePath ~startPath:(Filename.dirname startPath) name
