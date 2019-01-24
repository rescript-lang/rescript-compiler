let main() =
  let lexbuf = Lexing.from_channel stdin in
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "-html"
  then Htmltransf.main lexbuf
  else Transf.main lexbuf;
  exit 0;;

Printexc.print main ();;
