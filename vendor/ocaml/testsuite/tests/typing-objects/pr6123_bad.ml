class virtual name =
object
end

and func (args_ty, ret_ty) =
object(self)
  inherit name

  val mutable memo_args = None

  method arguments =
    match memo_args with
    | Some xs -> xs
    | None ->
      let args = List.map (fun ty -> new argument(self, ty)) args_ty in
        memo_args <- Some args; args
end

and argument (func, ty) =
object
  inherit name
end
;;
