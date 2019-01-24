let main () =
  Latexscan.main (Lexing.from_channel stdin);;

Printexc.print main (); exit 0;;
