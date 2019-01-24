(* these are not valid under -strict-formats, but we test them here
   for backward-compatibility *)

Printf.printf "%047.27d\n" 1036201459;;
Printf.printf "%047.27ld\n" 1036201459l;;
Printf.printf "%047.27Ld\n" 1036201459L;;
Printf.printf "%047.27nd\n" 1036201459n;;

print_newline ();;

Printf.printf "%047.27i\n" 1036201459;;
Printf.printf "%047.27li\n" 1036201459l;;
Printf.printf "%047.27Li\n" 1036201459L;;
Printf.printf "%047.27ni\n" 1036201459n;;

print_newline ();;

Printf.printf "%047.27u\n" 1036201459;;
Printf.printf "%047.27lu\n" 1036201459l;;
Printf.printf "%047.27Lu\n" 1036201459L;;
Printf.printf "%047.27nu\n" 1036201459n;;

print_newline ();;

Printf.printf "%047.27x\n" 1036201459;;
Printf.printf "%047.27lx\n" 1036201459l;;
Printf.printf "%047.27Lx\n" 1036201459L;;
Printf.printf "%047.27nx\n" 1036201459n;;

print_newline ();;

Printf.printf "%047.27X\n" 1036201459;;
Printf.printf "%047.27lX\n" 1036201459l;;
Printf.printf "%047.27LX\n" 1036201459L;;
Printf.printf "%047.27nX\n" 1036201459n;;

print_newline ();;

Printf.printf "%047.27o\n" 1036201459;;
Printf.printf "%047.27lo\n" 1036201459l;;
Printf.printf "%047.27Lo\n" 1036201459L;;
Printf.printf "%047.27no\n" 1036201459n;;
