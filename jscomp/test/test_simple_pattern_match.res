let (a, b) = switch Sys.os_type {
| "Unix"
| "Cygwin" => (1, 2)
| _ => (3, 4)
}
