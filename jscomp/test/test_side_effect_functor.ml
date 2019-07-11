include (
  struct
    module M () = struct
      let v = ref 0

      ;;
      incr v ;
      print_endline (string_of_int !v)

      let u = 3
      let use_v () = !v
      let unuse_v () = u + 32
    end

    module N = M ()

    (* let v = N.use_v *)
    let h = N.unuse_v
  end :
    sig
      val h : unit -> int
    end )
