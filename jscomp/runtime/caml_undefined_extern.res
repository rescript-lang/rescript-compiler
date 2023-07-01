type t<+'a>
external empty: t<'a> = "#undefined"
external return: 'a => t<'a> = "%identity"
external toOption: t<'a> => option<'a> = "#undefined_to_opt"
