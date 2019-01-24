(* A simple PPX *)

open Ast_mapper

let () =
  register "test" (fun _ ->
      Printf.eprintf "use_threads=%b\n" !Clflags.use_threads;
      Printf.eprintf "use_vmthreads=%b\n" !Clflags.use_vmthreads;
      default_mapper);

