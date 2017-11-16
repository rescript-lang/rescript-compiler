

val search:
  'poly_var -> 
  (int * 'a) array ->
  'a

val revSearch:  
  int -> 
  (int * string) array ->
  string ->
  'poly_var option

val toInt :   
  'variant -> int array -> int 

val fromInt :   
  int ->
  int array -> 
  int -> 
  'variant option 