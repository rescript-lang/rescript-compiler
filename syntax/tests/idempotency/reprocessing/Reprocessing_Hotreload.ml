module NoHotreloading = struct
  let checkRebuild _ _ =
    false
end

(* #if BYTECODE then *)
  include Reprocessing_Hotreload_Bytecode
(* #else *)
  include NoHotreloading
(* #end *)
