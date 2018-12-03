  type + 'a t 
  external empty : 'a t = "#undefined" 
  external return : 'a -> 'a t = "%identity"
  external toOption : 'a t -> 'a option = "#undefined_to_opt"