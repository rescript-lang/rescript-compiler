module type S = sig type 'a t end;;
module type T = S with type +'a t = 'a list;;
module type T = S with type -'a t = 'a list;;
