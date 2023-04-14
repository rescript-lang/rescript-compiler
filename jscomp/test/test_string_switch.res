let os_version = switch Sys.os_type {
| "Unix" => 1
| "Cygwin" => 2
| _ => 3
}
