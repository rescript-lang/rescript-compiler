include (
  {
    module M = Map.Make(String)
    let v = M.is_empty(M.empty)
  }: {
    let v: bool
  }
)
