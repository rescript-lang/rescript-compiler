let a, b = match Sys.os_type with "Unix" | "Cygwin" -> (1, 2) | _ -> (3, 4)
