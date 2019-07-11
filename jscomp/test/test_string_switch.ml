let os_version = match Sys.os_type with "Unix" -> 1 | "Cygwin" -> 2 | _ -> 3
